#!/bin/bash
ACC="AF086833"
# Make a directory for your ref file
mkdir -p ref
REF=ref/$ACC.fa
# Download the reference genome
efetch -db=nuccore -format=fasta -id=$ACC | seqret -filter -sid $ACC> $REF
# Download the sequencing data
SRR="SRR1553500"
fastq-dump -X 10000 SRR
# Index the reference genome
bwa index $REF
# Align the seqeunce data
# First, let us create some shortcuts:
r1={$SRR}_1.fastq
r2={$SRR}_2.fastq
TAG="@RG\tID:$SRR\tSM:$SRR\tLB:$SRR"
bwa mem -R $TAG $REF $r1 $r2 | samtools sorts > $SRR.bam
# Index the file
samtools index $SRR.bam
# Create genotype likelihood
samtools mpileup -uvf $REF SRR1553500.bam > genotypes.vcf
# The call the variants
bcftools call --ploidy 1 -vm -Ov genotypes.vcf > variants.vcf