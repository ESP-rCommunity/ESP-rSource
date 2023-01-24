#! /bin/bash

# This is the main script for automating results retrieval for the INDU-Zero models.

# There should be three models; semidetached, terraced and flat.
# When invoking this script, you can supply three command line arguments,
# pointing the script at these model directories respectively.
# If you do not supply command line arguments, the script will search for these
# directories and ask you to confirm.

# For each model, the script will:
# 1. Check for simulation results files as created by [model]/espy/
#    run_variants.py, run_variants_ph.py, and run_variants_ph_flins.py.
# 2. If results files are not found, run the appropriate script to generate them.
# 3. Call the script ./get_results_table.py, which reads results files and creates
#    a results table.

# First, check for help.
if [ "$1" == '-h' ] || [ "$1" == '--help' ]; then
    echo 'INDU-Zero.sh'
    echo
    echo 'Version 1.1 of October 2021.'
    echo
    echo 'This is the main script for automating results retrieval for the INDU-Zero models.'
    echo 'There should be three models; semidetached, terraced, and highrise flat.'
    echo 'You can specify the models on the command line. If you do not, the script will'
    echo 'search for likely models in the current directory, or prompt you for the model'
    echo 'directories.'
    echo
    echo 'This script requires a companion script "get_results_table.py" in the current'
    echo 'directory.'
    echo
    echo 'Usage:'
    echo './INDU-Zero.sh [-h, --help]'
    echo './INDU-Zero.sh [semidetached_model_dir terraced_model_dir flat_model_dir]'
    echo
    echo 'The script will search for existing results files in model directories. If any'
    echo 'required results files are not found for a model, this script will try to run the'
    echo 'script "[model_dir]/espy/run_variants.py".'
    exit
fi

# Next, check for command line arguments.
semi_dir=""
terr_dir=""
flat_dir=""
found=false
if [ "$1" != "" ] && [ "$2" != "" ] && [ "$3" != "" ]; then
    if [ -d "$1" ] && [ -d "$2" ] && [ -d "$3" ]; then
        echo 'Command line arguments verified.'
        semi_dir="$1"
        terr_dir="$2"
        flat_dir="$3"
        found=true
    else
        echo 'Invalid command line arguments.'
    fi
else
    echo 'Command line arguments not given.'
fi

# Now try scanning for model directories.
if ! $found; then
    for a in *semidetached*; do
        if [ -d "$a" ]; then
            echo "Found semidetached model directory $a."
            semi_dir="$a"
        fi
    done
    for a in *terrace*; do
        if [ -d "$a" ]; then
            echo "Found terraced model directory $a."
            terr_dir="$a"
        fi
    done
    for a in *highrise*; do
        if [ -d "$a" ]; then
            echo "Found highrise flat model directory $a."
            flat_dir="$a"
        fi
    done
    if [ "$semi_dir" != "" ] && [ "$terr_dir" != "" ] && [ "$flat_dir" != "" ]; then
        echo 'Do you wish to use these model directories? [y/n]'
        while true; do
            read yn
            if [ "$yn" == 'y' ]; then
                found=true
                break
            elif [ "$yn" == 'n' ]; then
                semi_dir=""
                terr_dir=""
                flat_dir=""
                break
            fi
            echo 'I didnt understand that, try again.'     
        done
    fi
fi

# As a last resort, prompt user for directories.
if ! $found; then
    if [ "$semi_dir" == "" ]; then
        while true; do
            echo 'Enter semidetached model directory.'
            read semi_dir
            if [ -d "$semi_dir" ]; then
                echo 'Accepted.'
                break
            fi
            echo 'Not a valid directory, try again.'
        done
    fi
    if [ "$terr_dir" == "" ]; then
        while true; do
            echo 'Enter terraced model directory.'
            read terr_dir
            if [ -d "$terr_dir" ]; then
                echo 'Accepted.'
                break
            fi
            echo 'Not a valid directory, try again.'
        done
    fi
    if [ "$flat_dir" == "" ]; then
        while true; do
            echo 'Enter highrise flat model directory.'
            read flat_dir
            if [ -d "$flat_dir" ]; then
                echo 'Accepted.'
                break
            fi
            echo 'Not a valid directory, try again.'
        done
    fi
fi

# We should now have all three model directories.
# Scan for results files.
model="$semi_dir"
echo
echo "Checking for files in semidetached model."
cd "$model"

files_pregen='nets/lighting_1H.csv nets/equipment_1H.csv nets/cooking_1H.csv nets/dhw_1H.csv'
files_res='res/semidetached.pkl res/semidetached_panelled.pkl'
files_res="$files_res res/semidetached_panelled_ph.pkl res/semidetached_panelled_ph_flins.pkl"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_GBR_11.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_GBR_35.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_GBR_26.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_GBR_20.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_GBR_49.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NLD_8.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NLD_16.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NLD_24.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NLD_32.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NLD_46.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NOR_16.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NOR_35.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NOR_40.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NOR_2.csv"
files_res="$files_res res/pv_gen_semidetached_panelled_ph_flins_NOR_39.csv"

echo 'Checking pre-generated files ...'
found=true
for f in $files_pregen; do
    if ! [ -f "$f" ]; then
        echo "Error: file $f not found."
        echo 'These files should be distributed with the model.'
        echo 'Exiting.'
        found=false
        break
    fi 
done
if ! $found; then
    exit 1
else
    echo 'Done.'
fi

echo 'Checking simulation results files ...'
while true; do
    found=true
    for f in $files_res; do
        if ! [ -f "$f" ]; then
            echo "File $f not found."
            found=false
            break
        fi 
    done
    if ! $found; then
        echo "Running script $model/espy/run_variants.py"
        python 'espy/run_variants.py'
        if [ $? -ne 0 ]; then
            echo 'Error: script failed.'
            exit 1
        fi
    else
        echo 'Done.'
        break
    fi
done

cd ..

model="$terr_dir"
echo
echo "Checking for files in terrace model."
cd "$model"

files_pregen='nets/lighting_1H.csv nets/equipment_1H.csv nets/cooking_1H.csv nets/dhw_1H.csv'
files_res='res/terrace.pkl res/terrace_panelled.pkl'
files_res="$files_res res/terrace_panelled_ph.pkl res/terrace_panelled_ph_flins.pkl"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_GBR_11.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_GBR_35.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_GBR_26.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_GBR_20.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_GBR_49.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NLD_8.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NLD_16.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NLD_24.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NLD_32.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NLD_46.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NOR_16.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NOR_35.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NOR_40.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NOR_2.csv"
files_res="$files_res res/pv_gen_terrace_panelled_ph_flins_NOR_39.csv"

echo 'Checking pre-generated files ...'
found=true
for f in $files_pregen; do
    if ! [ -f "$f" ]; then
        echo "Error: file $f not found."
        echo 'These files should be distributed with the model.'
        echo 'Exiting.'
        found=false
        break
    fi 
done
if ! $found; then
    exit 1
else
    echo 'Done.'
fi

echo 'Checking simulation results files ...'
while true; do
    found=true
    for f in $files_res; do
        if ! [ -f "$f" ]; then
            echo "File $f not found."
            found=false
            break
        fi 
    done
    if ! $found; then
        echo "Running script $model/espy/run_variants.py"
        python 'espy/run_variants.py'
        if [ $? -ne 0 ]; then
            echo 'Error: script failed.'
            exit 1
        fi
    else
        echo 'Done.'
        break
    fi
done

cd ..

model="$flat_dir"
echo
echo "Checking for files in highrise flat model."
cd "$model"

files_pregen='nets/lighting_1H.csv nets/equipment_1H.csv nets/cooking_1H.csv nets/dhw_1H.csv'
files_res='res/highrise.pkl res/highrise_panelled.pkl'
files_res="$files_res res/highrise_panelled_ph.pkl res/highrise_panelled_ph_flins.pkl"

echo 'Checking pre-generated files ...'
found=true
for f in $files_pregen; do
    if ! [ -f "$f" ]; then
        echo "Error: file $f not found."
        echo 'These files should be distributed with the model.'
        echo 'Exiting.'
        found=false
        break
    fi 
done
if ! $found; then
    exit 1
else
    echo 'Done.'
fi

echo 'Checking simulation results files ...'
while true; do
    found=true
    for f in $files_res; do
        if ! [ -f "$f" ]; then
            echo "File $f not found."
            found=false
            break
        fi 
    done
    if ! $found; then
        echo "Running script $model/espy/run_variants.py"
        python 'espy/run_variants.py'
        if [ $? -ne 0 ]; then
            echo 'Error: script failed.'
            exit 1
        fi
    else
        echo 'Done.'
        break
    fi
done

cd ..

# Now call python script to generate results table.
echo 
echo 'Creating results table ...'
python3 get_results_table.py "$semi_dir" "$terr_dir" "$flat_dir"
if [ $? -eq 0 ]; then
    echo 
    echo 'Done. Results table output to ./results_table.txt.'
else
    echo
    echo 'Error: script ./get_results_table.py failed.'
fi
