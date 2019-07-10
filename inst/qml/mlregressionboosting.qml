//
// Copyright (C) 2013-2019 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Theme 1.0

Form {
    usesJaspResults: true

    VariablesForm
    {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList  { 
            id: target
            name: "target"     
            title: qsTr("Target")         
            singleVariable: true
            allowedColumns: ["scale"]  
        }
        AssignedVariablesList { 
            id: predictors
            name: "predictors" 
            title: qsTr("Predictors")
            allowedColumns: ["scale", "nominal", "ordinal", "nominalText"] 
        }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { name: "regBoostRelInfTable";	text: qsTr("Relative influence")            }
    }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { name: "plotRelInf"      ; text: qsTr("Relative influence")       }
        CheckBox { name: "plotDeviance"    ; text: qsTr("Deviance")                 }
        CheckBox { name: "plotOOBChangeDev"; text: qsTr("OOB improvement")          }
        CheckBox { name: "plotPredPerf"    ; text: qsTr("Predictive performance")   }
    }

    Section {
        text: qsTr("Training Parameters")
        columns: 2

        RadioButtonGroup {
            title: qsTr("Model Optimization")
            name: "modelOptimization"
            RadioButton { name: "cv"; childrenOnSameRow: true
                IntegerField {
                    name: "cvFolds"
                    afterLabel: qsTr("-fold cross-validation")
                    defaultValue: 3
                    min: 2
                    max: 30
                    fieldWidth: 25
            }
            }
            RadioButton { text: qsTr("Out-of-bag")              ; name: "oob"                  }
            RadioButton { text: qsTr("None")                    ; name: "noOpt"; checked: true }
        }

        GroupBox {
            DoubleField  { name: "noOfTrees"   ; text: qsTr("Number of trees for training:") ; defaultValue: 100 ; min: 1     ; max: 999999; fieldWidth: 60 }
            DoubleField  { name: "shrinkage"   ; text: qsTr("Shrinkage:")                    ; defaultValue: 0.1; min: 0; max: 1; fieldWidth: 60;           }
            IntegerField { name: "int.depth"   ; text: qsTr("Interaction depth:")            ; defaultValue: 1; min: 1; max: 99; fieldWidth: 60;            }
            IntegerField { name: "nNode"       ; text: qsTr("Min. no. observations in node:"); defaultValue: 10; min: 1; max: 999999; fieldWidth: 60;       }
            DropDown {
                name: "dist"
                indexDefaultValue: 0
                label: qsTr("Loss function:")
                values:
                [
                    { label: "Gaussian", value: "gaussian"},
                    { label: "Laplace", value: "laplace"},
                    { label: "t", value: "tdist"}
                ]
            }
            PercentField { name: "dataTrain"   ; text: qsTr("Data used for training:")       ; defaultValue: 80                                             }
            PercentField { name: "bag.fraction"; text: qsTr("Training data used per tree:")  ; defaultValue: 50                                             }
            CheckBox     { name: "seedBox"     ; text: qsTr("Set seed:")                     ; childrenOnSameRow: true
                DoubleField  { name: "seed"; defaultValue: 1; min: -999999; max: 999999; fieldWidth: 60 }
            }
        }
    }

    Section {
        text: qsTr("Predictions")
        debug: true

        RadioButtonGroup
        {
            name: "applyModel"
            RadioButton { value: "noApp"         ; text: qsTr("Do not predict data"); checked: true                            }
            RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target")                              }
            RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to apply indicator"); id: applyIndicator }
        }

        VariablesForm {
        visible: applyIndicator.checked
            height: 150
            AvailableVariablesList { name: "predictionVariables"; allowedColumns: ["nominal"] }
            AssignedVariablesList {
                        name: "indicator"
                        title: qsTr("Apply indicator")
                        singleVariable: true
                        allowedColumns: ["nominal"]
            }
        }
    }

    Item 
    {
        height: 			saveModel.height
        Layout.fillWidth: 	true
        Layout.columnSpan: 2

        Button 
        {
            id: 			saveModel
            anchors.right: 	parent.right
            text: 			qsTr("<b>Save Model</b>")
            enabled: 		predictors.count > 1 && target.count > 0
            onClicked:      { }
            debug: true	
        }
    }

}
