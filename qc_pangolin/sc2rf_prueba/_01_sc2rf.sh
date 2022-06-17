#conda activate sc2rf
cd /home/alberto.lema/Desktop/Programas/sc2rf

./sc2rf.py --parents 1-35 --breakpoints 0-100 --unique 1 --max-ambiguous 10000 -d --show-private-mutations --force-all-parents --csvfile /home/alberto.lema/Documents/Desarrollo/relecov_qc_exercise/qc_pangolin/sc2rf_prueba/sc2rf.csv /home/alberto.lema/Documents/Desarrollo/relecov_qc_exercise/qc_pangolin/sc2rf_prueba/align_multi_muestra_1.fa > /home/alberto.lema/Documents/Desarrollo/relecov_qc_exercise/qc_pangolin/sc2rf_prueba/sc2rf.out
