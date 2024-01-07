#!/bin/bash

## Ange Zoclanclounon, PhD
## Bioinformatics Scientist @Rothamsted Research
## X: @angeomics

## Usage: ./run_kallisto.sh transcript_file.fasta trimmed_reads_path outputdirectory

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 transcript_file.fasta trimmed_reads_path outputdirectory"
    exit 1
fi

transcript_file="$1"
trimmed_reads_path="$2"
outputdirectory="$3"

## Activate the kallisto environment
conda activate kallisto_env

## Index the transcript
kallisto index --index=$(basename "$transcript_file" .fasta)_index "$transcript_file"

## Quantification
for fq_trim1 in "$trimmed_reads_path"/*_1.fastq.gz
do
    echo "Working with file $fq_trim1"  
    base=$(basename "$fq_trim1" _1.fastq.gz)
    echo "Base name is $base"
    fq_trim1="$trimmed_reads_path/${base}_1.fastq.gz"
    fq_trim2="$trimmed_reads_path/${base}_2.fastq.gz"
    echo "Quantification with kallisto"
    kallisto quant --index=$(basename "$transcript_file" .fasta)_index --output-dir="$outputdirectory/$base" --threads=4 --plaintext "$fq_trim1" "$fq_trim2"
done

## Create a single TSV file that has the TPM abundance estimates for all samples.
cd "$outputdirectory"
paste */abundance.tsv | cut -f 1,2,5,10,15,20,25,30 > transcript_tpms_all_samples.tsv
ls -1 */abundance.tsv | awk -F'/' '{print $1}' | awk '{printf "target_id\tlength\t%s\n", $0}' > header.tsv
cat header.tsv transcript_tpms_all_samples.tsv | grep -v "tpm" > transcript_tpms_all_samples.tsv2
mv transcript_tpms_all_samples.tsv2 transcript_tpms_all_samples.tsv
rm -f header.tsv

## Deactivate the kallisto environment
conda deactivate
