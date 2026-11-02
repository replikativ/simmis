# ContractNLI: A Dataset for Document-level Natural Language Inference for Contracts

ContractNLI is a dataset for document-level natural language inference (NLI) on contracts whose goal is to automate/support a time-consuming procedure of contract review.
In this task, a system is given a set of hypotheses (such as "Some obligations of Agreement may survive termination.") and a contract, and it is asked to classify whether each hypothesis is _entailed by_, _contradicting to_ or _not mentioned by_ (neutral to) the contract as well as identifying _evidence_ for the decision as spans in the contract.

ContractNLI is the first dataset to utilize NLI for contracts and is also the largest corpus of annotated contracts (as of September 2021).
ContractNLI is an interesting challenge to work on from a machine learning perspective (the label distribution is imbalanced and it is naturally multi-task, all the while training data being scarce) and from a linguistic perspective (linguistic characteristics of contracts, particularly negations by exceptions, make the problem difficult).

Details of ContractNLI can be found in our paper that was published in "Findings of EMNLP 2021".
If you have a question regarding our dataset, you can contact us by emailing koreeda@stanford.edu or by creating an issue in this repository.

## Dataset specification

More formally, the task consists of:
* **Natural language inference (NLI)**: Document-level three-class classification (one of `Entailment`, `Contradiction` or `NotMentioned`).
* **Evidence identification**: Multi-label binary classification over _span_s, where a _span_ is a sentence or a list item within a sentence. This is only defined when NLI label is either `Entailment` or `Contradiction`. Evidence spans need not be contiguous but need to be comprehensively identified where they are redundant.

We have 17 hypotheses annotated on 607 non-disclosure agreements (NDAs).
The hypotheses are fixed throughout all the contracts including the test dataset.

Our dataset is provided as JSON files.

```json
{
  "documents": [
    {
      "id": 1,
      "file_name": "example.pdf",
      "text": "NON-DISCLOSURE AGREEMENT\nThis NON-DISCLOSURE AGREEMENT (\"Agreement\") is entered into this ...",
      "document_type": "search-pdf",
      "url": "https://examplecontract.com/example.pdf",
      "spans": [
        [0, 24],
        [25, 89],
        ...
      ],
      "annotation_sets": [
        {
          "annotations": {
            "nda-1": {
              "choice": "Entailment",
              "spans": [
                12,
                13,
                91
              ]
            },
            "nda-2": {
              "choice": "NotMentioned",
              "spans": []
            },
            ...
          }
        }
      ]
    },
    ...
  ],
  "labels": {
    "nda-1": {
      "short_description": "Explicit identification",
      "hypothesis": "All Confidential Information shall be expressly identified by the Disclosing Party."
    },
    ...
  }
}
```

The core information in our dataset is:
* `text`: The full document text
* `spans`: List of spans as pairs of the start and end character indices.
* `annotation_sets`: It is provided as a list to accommodate multiple annotations per document. Since we only have a single annotation for each document, you may safely access the appropriate annotation by `document['annotation_sets'][0]['annotations']`.
* `annotations`: Each key represents a hypothesis key. `choice` is either `Entailment`, `Contradiction` or `NotMentioned`. `spans` is given as indices of `spans` above. `spans` is empty when `choice` is `NotMentioned`.
* `labels`: Each key represents a hypothesis key. `hypothesis` is the hypothesis text that should be used in NLI.

The JSON file comes with supplemental information. Users may simply ignore the information if you are only interested in developing machine learning systems.
* `id`: A unique ID throughout train, development and test datasets.
* `file_name`: The filename of the original document in the dataset zip file.
* `document_type`: One of `search-pdf` (a PDF from a search engine), `sec-text` (a text file from SEC filing) or `sec-html` (an HTML file from SEC filing).
* `url`: The URL that we obtained the document from.


## Baseline system

In our paper, we introduced Span NLI BERT, a strong baseline for our task.
It (1) makes the problem of evidence identification easier by modeling the problem as multi-label classification over spans instead of trying to predict the start and end tokens, and (b) introduces more sophisticated context segmentation to deal with long documents.
We showed in our paper that Span NLI BERT significantly outperforms the existing models.

You can find the implementation of Span NLI BERT in [another repository](https://github.com/stanfordnlp/contract-nli-bert).

## License

Our dataset is released under CC BY 4.0.
Please refer attached "[LICENSE](./LICENSE)" or https://creativecommons.org/licenses/by/4.0/ for the exact terms.

When you use our dataset in your work, please cite our paper:

```bibtex
@inproceedings{koreeda-manning-2021-contractnli,
    title = "ContractNLI: A Dataset for Document-level Natural Language Inference for Contracts",
    author = "Koreeda, Yuta  and
      Manning, Christopher D.",
    booktitle = "Findings of the Association for Computational Linguistics: EMNLP 2021",
    year = "2021",
    publisher = "Association for Computational Linguistics"
}
```

## Changelog and release note

* 10/5/2021: Initial release
