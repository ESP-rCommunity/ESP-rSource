"""Get results table."""

import os
import sys
import pickle
import numpy as np
import pandas as pd

def print_list_range(list_in):
    """Print numbers in list as a range."""
    print(
        f"{(0.5*(max(list_in)+min(list_in))):.1f} ± {(0.5*(max(list_in)-min(list_in))):.1f}",
        end="",
    )
    
def get_list_range(list_in):
    """Process numbers in list to mean and range."""
    out = [0.0,0.0]
    out[0] = 0.5*(max(list_in)+min(list_in))
    out[1] = 0.5*(max([abs(x) for x in list_in])-min([abs(x) for x in list_in]))
    return out

def get_ashp_elec_use(energy_demand, area):
    """Calculate DCU electrical energy use from demand (in kWh/m2)."""
    q_h = [2.5, 5, 10, 20, 40, 60, 80, 100]
    eta_low = [2.648, 2.840, 3.090, 3.276, 3.382, 3.412, 3.437, 3.452]
    F_low = [0.997, 0.961, 0.828, 0.578, 0.342, 0.240, 0.183, 0.149]
    eta_high = [2.712, 2.865, 3.142, 3.370, 3.497, 3.537, 3.559, 3.571]
    F_high = [1, 0.984, 0.902, 0.674, 0.406, 0.286, 0.219, 0.178]

    energy_demand_per_area_MJ = energy_demand * 3.6
    energy_demand_GJ = energy_demand * area * 0.0036

    if energy_demand_per_area_MJ <= 150:
        eta = np.interp(energy_demand_GJ, q_h, eta_low)
        F = np.interp(energy_demand_GJ, q_h, F_low)
    else:
        eta = np.interp(energy_demand_GJ, q_h, eta_high)
        F = np.interp(energy_demand_GJ, q_h, F_high)

    elec_energy_use_ashp = (F * energy_demand) / eta
    elec_energy_use_aux = (1 - F) * energy_demand
    return elec_energy_use_ashp + elec_energy_use_aux

def load_energy_res(f_in):
    """Load results in correct order from pkl file."""
    with open(f_in, "rb") as f:
        energy_sims = pickle.load(f)

    gla = energy_sims.pop(0)
    ams = energy_sims.pop(0)
    osl = energy_sims.pop(0)
    energy_sims.insert(0, ams)
    energy_sims.insert(1, gla)
    energy_sims.insert(2, osl)
    return energy_sims

def print_line(f_out,s):
    """Print an output line to file and stdout."""
    print(s)
    f_out.write(s+'\n')

def main():
    """Main function to print results table."""

    # Should have three command line arguments with model directories.
    if len(sys.argv) != 4:
        print('Error: need 3 command line arguments.')
        sys.exit()
    models = sys.argv[1:]
    names = ['semidetached','terrace','highrise']

    i=0
    energy_lights=[0.0,0.0,0.0]
    energy_appliances=[0.0,0.0,0.0]
    energy_cooking=[0.0,0.0,0.0]
    energy_dhw=[0.0,0.0,0.0]
    energy_pv=[[],[],[]]
    energy_heat=[[],[],[]]
    energy_ashp=[[],[],[]]
    energy_elec=[[],[],[]]
    energy_net=[[],[],[]]

    for model,name in zip(models,names):

        cfg_file_tmps = [
            name+".cfg.tmp",  # pre-upgrade (base case)
            name+"_panelled.cfg.tmp",  # + concept panels (notb, walls, roof, glazing, doors)
            name+"_panelled_ph.cfg.tmp",  # + PH airtightness + MVHR 75%
            name+"_panelled_ph_flins.cfg.tmp",  # + floor insulation
        ]
        os.chdir(model+"/cfg")

        area_tf = 94.6  # Treated floor area

        # LIGHTING AND APPLIANCES
        f_light = "../nets/lighting_1H.csv"
        f_equip = "../nets/equipment_1H.csv"
        f_cook = "../nets/cooking_1H.csv"
        tsph_light = 1
        tsph_equip = 1
        tsph_cook = 1
        df_lights = pd.read_csv(f_light, sep=",", header=0, index_col=0, parse_dates=True)
        df_equip = pd.read_csv(f_equip, sep=",", header=0, index_col=0, parse_dates=True)
        df_cook = pd.read_csv(f_cook, sep=",", header=0, index_col=0, parse_dates=True)
        energy_lights[i] = (
            df_lights.sum(axis=1).sum() * (3600 / tsph_light) / (3.6e6) / area_tf
        )
        energy_appliances[i] = (
            df_equip.sum(axis=1).sum() * (3600 / tsph_equip) / (3.6e6) / area_tf
        )
        energy_cooking[i] = df_cook.sum(axis=1).sum() * (3600 / tsph_cook) / (3.6e6) / area_tf
        # print(f"Lighting electrical use: {energy_lights:.1f} kWh/m\u00B2")
        # print(f"Non-cooking appliance electrical use: {energy_appliances:.1f} kWh/m\u00B2")
        # print(f"Cooking appliance electrical use: {energy_cooking:.1f} kWh/m\u00B2")

        # DOMESTIC HOT WATER
        f_dhw = "../nets/dhw_1H.csv"
        delta_t_dhw = 37
        specific_heat = 4180
        df_dhw = pd.read_csv(f_dhw, sep=",", header=0, index_col=0, parse_dates=True)
        energy_dhw[i] = (
            df_dhw.sum(axis=1).sum() * delta_t_dhw * specific_heat / (3.6e6) / area_tf
        )
        # print(f"DHW electrical use: {energy_dhw:.1f} kWh/m\u00B2")

        # PV GEN
        if i<2:
            tsph_sim = 1
            locations = [
                "../dbs/GBR_Glasgow_ECMWF_tmy_2007_2016_syn",
                "../dbs/NLD_Amsterdam_ECMWF_tmy_2007_2016_syn",
                "../dbs/NOR_Oslo_ECMWF_tmy_2007_2016_syn",
            ]
            subsets = [[10, 34, 25, 19, 48], [7, 15, 23, 31, 45], [15, 34, 39, 1, 38]]
            for j, loc in enumerate(locations):
                energy_pv[i].append([])
                for k in range(50):
                    if k in subsets[j]:
                        # read from last variant as PV results do not change
                        df_elr = pd.read_csv(
                            f"../res/pv_gen_{cfg_file_tmps[3][:-8]}_{loc[7:10]}_{k + 1}.csv",
                            names=["Time", "Gen(W)"],
                            index_col=0,
                            usecols=[0, 1],
                            skiprows=2,
                        )
                        energy_pv[i][j].append(
                            -df_elr.sum()[0] * (3600 / tsph_sim) / (3.6e6) / area_tf
                        )
            gla = energy_pv[i].pop(0)
            ams = energy_pv[i].pop(0)
            osl = energy_pv[i].pop(0)
            energy_pv[i].insert(0, ams)  # mean 59.7 kWh/m2
            energy_pv[i].insert(1, gla)  # mean 45 kWh/m2
            energy_pv[i].insert(2, osl)  # mean 49 kWh/m2
        else:
            energy_pv[i] = [[0.0,0.0,0.0,0.0,0.0],[0.0,0.0,0.0,0.0,0.0],[0.0,0.0,0.0,0.0,0.0]]

        # GET SPACE HEATING (ASHP) and CALC NET ENERGY AT METER
        for j,test in enumerate(cfg_file_tmps):
            energy_heat[i].append(load_energy_res(f"../res/{test[:-8]}.pkl"))
            energy_ashp[i].append([])
            energy_elec[i].append([])
            energy_net[i].append([])
            for k, e_loc_i in enumerate(energy_heat[i][j]):
                energy_ashp[i][j].append([])
                energy_ashp[i][j][k] = [get_ashp_elec_use(x, area_tf) for x in e_loc_i]
                energy_elec[i][j].append([])
                energy_elec[i][j][k] = [
                    x + energy_lights[i] + energy_appliances[i] + energy_cooking[i] + energy_dhw[i]
                    for x in energy_ashp[i][j][k]
                ]
                energy_net[i][j].append([])
                energy_net[i][j][k] = [x + y for x, y in zip(energy_elec[i][j][k], energy_pv[i][k])]

        # print("debug")
        i += 1
        os.chdir("../..")

    # Debug.
    # print(energy_lights)
    # print(energy_appliances)
    # print(energy_cooking)
    # print(energy_dhw)
    # print(energy_pv)
    # print(energy_heat)
    # print(energy_ashp)
    # print(energy_elec)
    # print(energy_net)

    # Now, write the table.
    names=[['semidetached',''],['terraced',''],['high rise','flat']]
    locations=['amsterdam','glasgow','oslo']
    cases=[['base',''],['+ upgrade','  panels'],['+ passivhaus','  ventilation'],['+ floor','  insulation']]
    for i in range(3):
        for j in range(3):
            energy_pv[i][j] = get_list_range(energy_pv[i][j])
            for k in range(4):
                energy_heat[i][k][j] = get_list_range(energy_heat[i][k][j])
                energy_ashp[i][k][j] = get_list_range(energy_ashp[i][k][j])
                energy_elec[i][k][j] = get_list_range(energy_elec[i][k][j])
                energy_net[i][k][j] = get_list_range(energy_net[i][k][j])

    # Open file for results table and print header.
    f_out=open('results_table.txt','w')
    print_line(f_out,'Results of ESP-r simulations of INDU-Zero archetype variations under three different climates.')
    print_line(f_out,'')
    print_line(f_out,'All values in units of kWh/m^2.')
    print_line(f_out,'Where a range is reported, values are mean ± range/2 from five different weather variations.')
    print_line(f_out,'')
    print_line(f_out,'|              |           |               | space   |                             electrical draws                                |')
    print_line(f_out,'| archetype    | location  | case          | heating | lighting | appliances | cooking | dhw   | ashp   | total  | pv     | net    |')
    print_line(f_out,'|--------------|-----------|---------------|---------|----------|------------|---------|-------|--------|--------|--------|--------|')
    for i in range(3):
        print_line(f_out,'| {:12s} | {:9s} | {:13s} | {:7.1f} | {:8.1f} | {:10.1f} | {:7.1f} | {:5.1f} | {:6.1f} | {:6.1f} | {:6.1f} | {:6.1f} |'.format(
            names[i][0],
            locations[0],
            cases[0][0],
            energy_heat[i][0][0][0],
            energy_lights[i],
            energy_appliances[i],
            energy_cooking[i],
            energy_dhw[i],
            energy_ashp[i][0][0][0],
            energy_elec[i][0][0][0],
            energy_pv[i][0][0],
            energy_net[i][0][0][0]
        ))
        print_line(f_out,'| {:12s} |           |               | ± {:5.1f} |          |            |         |       | ± {:4.1f} | ± {:4.1f} | ± {:4.1f} | ± {:4.1f} |'.format(
            names[i][1],
            energy_heat[i][0][0][1],
            energy_ashp[i][0][0][1],
            energy_elec[i][0][0][1],
            energy_pv[i][0][1],
            energy_net[i][0][0][1]
        ))
        for k in range(1,4):
            print_line(f_out,'|              |           | {:13s} | {:7.1f} |          |            |         |       | {:6.1f} | {:6.1f} |        | {:6.1f} |'.format(
                cases[k][0],
                energy_heat[i][k][0][0],
                energy_ashp[i][k][0][0],
                energy_elec[i][k][0][0],
                energy_net[i][k][0][0]
            ))
            print_line(f_out,'|              |           | {:13s} | ± {:5.1f} |          |            |         |       | ± {:4.1f} | ± {:4.1f} |        | ± {:4.1f} |'.format(
                cases[k][1],
                energy_heat[i][k][0][1],
                energy_ashp[i][k][0][1],
                energy_elec[i][k][0][1],
                energy_net[i][k][0][1]
            ))
        for j in range(1,3):
            print_line(f_out,'|              | {:9s} | {:13s} | {:7.1f} |          |            |         |       | {:6.1f} | {:6.1f} | {:6.1f} | {:6.1f} |'.format(
                locations[j],
                cases[0][0],
                energy_heat[i][0][j][0],
                energy_ashp[i][0][j][0],
                energy_elec[i][0][j][0],
                energy_pv[i][j][0],
                energy_net[i][0][j][0]
            ))
            print_line(f_out,'|              |           | {:13s} | ± {:5.1f} |          |            |         |       | ± {:4.1f} | ± {:4.1f} | ± {:4.1f} | ± {:4.1f} |'.format(
                cases[0][1],
                energy_heat[i][0][j][1],
                energy_ashp[i][0][j][1],
                energy_elec[i][0][j][1],
                energy_pv[i][j][1],
                energy_net[i][0][j][1]
            ))
            for k in range(1,4):
                print_line(f_out,'|              |           | {:13s} | {:7.1f} |          |            |         |       | {:6.1f} | {:6.1f} |        | {:6.1f} |'.format(
                    cases[k][0],
                    energy_heat[i][k][j][0],
                    energy_ashp[i][k][j][0],
                    energy_elec[i][k][j][0],
                    energy_net[i][k][j][0]
                ))
                print_line(f_out,'|              |           | {:13s} | ± {:5.1f} |          |            |         |       | ± {:4.1f} | ± {:4.1f} |        | ± {:4.1f} |'.format(
                    cases[k][1],
                    energy_heat[i][k][j][1],
                    energy_ashp[i][k][j][1],
                    energy_elec[i][k][j][1],
                    energy_net[i][k][j][1]
                ))                
        print_line(f_out,'|--------------|-----------|---------------|---------|----------|------------|---------|-------|--------|--------|--------|--------|')
    
    f_out.close()

if __name__ == "__main__":
    main()
