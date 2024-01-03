#!/bin/bash

## Ange Zoclanclounon, PhD
## Bioinformatics Scientist @Rothamsted Research
## X : @angeomics


## Quick run: ./fastp.sh /path/to/your/rnaseq/data



## RNA-Seq QC analysis with FasQC, fastp, and multiQC tools
### Installation 
### Create an environment and install FasQC, fastp, and multiQC tools

# conda create --yes -n qc_env fastp fastqc multiqc

###--Data quality check and trimming

function show_help {
    echo "Usage: $0 <input_directory>"
    echo ""
    echo "This script performs RNA-Seq QC analysis with FasQC, fastp, and multiQC tools."
    echo "The input directory should contain the RNA-seq data in fastq.gz format."
    echo ""
    exit 1
}

if [ "$#" -eq 0 ]; then
    show_help
fi

if [ "$1" == "--help" ] || [ "$1" == "-h" ]; then
    show_help
fi

input_directory=$1

echo "#---FastQC quality check ongoing---#"

conda activate qc_env

cd "$input_directory" || exit 1

mkdir -p qc1  # Use -p to avoid an error if the directory already exists

fastqc -o qc1 -t 16 *.fastq.gz 

echo "#---Adapter trimming by fastp ongoing---#"

mkdir -p adap  # Use -p to avoid an error if the directory already exists

for reads1 in *_R1_001.fastq.gz
do
    base=$(basename "$reads1" _R1_001.fastq.gz)
    fastp --detect_adapter_for_pe \
          --overrepresentation_analysis \
          --correction --cut_right --thread 2 \
          --html adap/${base}.fastp.html --json adap/${base}.fastp.json \
          -i ${base}_R1_001.fastq.gz -I ${base}_R2_001.fastq.gz \
          -o adap/${base}_1.fastq.gz -O adap/${base}_2.fastq.gz
done

echo "#---Quality control of pre- and post-trimmed reads with multiQC---#"

multiqc qc1/ adap/

conda deactivate

echo "Data QC is completed!"
