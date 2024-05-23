# Rscript sample_books.R -i "C:\Users\User\detcorpus-entities\metadata.csv" -l 1920 -u 1939 -t "C:\Users\User\detcorpus-entities" -p "C:\Users\User\regionalism\plots\histogram.jpg" -o "C:\Users\User\regionalism\data\sample_metadata.csv"
Rscript count_toponyms.R -m "C:\Users\User\regionalism\data\sample_metadata.csv" -t "C:\Users\User\regionalism\data\unique_toponyms.csv" -o "C:\Users\User\regionalism\data\n_toponyms.csv" -q "C:\Users\User\regionalism\src\geocode_toponyms.sh"