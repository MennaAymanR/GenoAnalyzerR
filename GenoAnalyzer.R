

transcription<- function(user_input)
{
  for(i in user_input)
    {
    if(i=="A")
      {
      print("U")
    }
    ifelse(i=="T")
    {
      print("A")
    }
    ifelse(i=="C")
    {
      print("G")
    }
    ifelse(i=="G")
    {
      print("C")
    }
    ifelse(i=="N")
    {
      print("AnyNucleotide")
    }
  }
}
translate<- function(user_input)
{
  codons<- c("ATA","ATC","ATT","ATG","ACA","ACC","ACG","ACT",
             "AAC","AAT","AAA","AAG","AGC","AGT","AGA","AGG",
             "CTA","CTC","CTG","CTT","CCA","CCC","CCG","CCT",
             "CAC","CAT","CAA","CAG","CGA","CGC","CGG","CGT",
             "GTA","GTC","GTG","GTT","GCA","GCC","GCG","GCT",
             "GAC","GAT","GAA","GAG","GGA","GGC","GGG","GGT",
             "TCA","TCC","TCG","TCT","TTC","TTT","TTA","TTG",
             "TAC","TAT","TAA","TAG","TGC","TGT","TGA","TGG")
  codonsname<- c("I","I","I","M","T","T","T",
                 "T","N","N","K","K",
                 "S","S","R","R","L",
                 "L","L","L","P","P",
                 "P","P","H","H","Q",
                 "Q","R","R","R","V",
                 "V","V","V","A","A",
                 "A","A","D","D","E",
                 "E","G","G","G","G",
                 "S","S","S","S","F",
                 "F","L","L","Y","Y",
                 "_","_","C","C","_","W")
  protein<-c(" ")
  for(j in codons)
    {
    for (k in codonsname)
    {
      codons[j]=codonsname[k]
    }
  }
  
}
filter<-function(user_input)
{
filterseq=replace("N",input_user,"")
print(filterseq)
}
start_code<-function(user_input)
  {
  for(i in user_input)
    {
    if (user_input[i]=="A")
      {
      if(user_input[i+2]=="T")
        {
        if(user_input[i+3]=="G")
          {
          print(i)
        }
      }
    }
  }
}
eng_name<-function()
  {
  print("developed by menna ayman")
}
user_input<-readline(prompt = "please enter your input, path or seq?")


if(user_input=="seq")
{
  print(seq)
} 
ifelse(user_input=="path")
{
  x<-readline(prompt = "please enter your path:")
  y<-import.fasta(x, aa.to.upper = TRUE, gap.to.dash = TRUE, log.file = NULL)
  print(x)
  
}
z<-readline(prompt = "what's your process?transcripe or translate or filter or start_code or eng_name:")
if(z=="transcripe")
{
  print(z)
}
ifelse(z=="translate")
{
  transcription(seq)
}
ifelse(z=="filter")
{
  filter(seq)
}
ifelse(z=="start_codon")
{
 start_code(seq) 
}
ifelse(z=="eng_name")
{
  eng_name()
}