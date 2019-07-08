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

// All Analysis forms must be built with the From QML item
Form
{

    VariablesForm {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList  { name: "target"    ; title: qsTr("Target")         ; singleVariable: true; allowedColumns: ["nominal", "nominalText", "ordinal"]; id: target }
        AssignedVariablesList  { name: "predictors"; title: qsTr("Predictors"); allowedColumns: ["scale", "ordinal"]; id: predictors                                        }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { text: qsTr("Confusion matrix") ; name: "confusionTable"; checked: true
          CheckBox { text: qsTr("Proportions"); name: "confusionProportions"} } 
        CheckBox { name: "manovaTable";       text: qsTr("MANOVA")             }
        CheckBox { name: "coefficientsTable";   text: qsTr("Coefficients")       }
        CheckBox { name: "priorTable";	    text: qsTr("Prior and posterior probabilities")            }
        CheckBox { name: "meanTable";       text: qsTr("Group means training data")             }
        }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { name: "rocCurve"; text: qsTr("ROC curve") }
        CheckBox { name: "matrixplot"
            text: qsTr("Discriminant matrix")

            RowLayout {
                CheckBox { name: "plotDensities"; text:qsTr("Densities"); checked: true}
                CheckBox { name: "plotStatistics"; text: qsTr("Scatter plots"); checked: true}
            }
        }
    }

    Section
    {
        title: qsTr("Training Parameters")

        ColumnLayout{

            RadioButtonGroup {
                title: qsTr("Model Optimization")
                name: "modelOpt"
                RadioButton { text: qsTr("Manual") ; name: "optimizationManual"; checked: true }
            }
            RadioButtonGroup {
                title: qsTr("Cross Validation")
                name: "modelValid"
                RadioButton { text: qsTr("None") ; name: "validationManual"; checked: true }
            }

        }

        GroupBox {

            PercentField { name: "trainingDataManual"; text: qsTr("Data used for training:")       ; defaultValue: 80    }
            DropDown {
                name: "estimationMethod"
                indexDefaultValue: 0
                label: qsTr("Estimation method")
                values:
                [
                    { label: "Moment", value: "moment"},
                    { label: "MLE", value: "mle"},
                    { label: "MVE", value: "covMve"},
                    { label: "t", value: "t"},
                ]
            }

            CheckBox { name: "seedBox"; text: qsTr("Set seed: "); childrenOnSameRow: true; checked: true
                    DoubleField  { name: "seed"; defaultValue: 1; min: -999999; max: 999999; fieldWidth: 60 } }
        }
    }

    // Section {
    //     title: qsTr("Predictions")
    //     debug: true
    //     Group {


    //         RadioButtonGroup
    //         {
    //             name: "applyModel"
    //             RadioButton { value: "noApp"         ; text: qsTr("Do not predict data"); checked: true; debug: true        }
    //             RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target"); debug: true  }
    //             RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to apply indicator"); debug: true       }

    //                }
    //             }
    // }
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
            enabled: 		predictors.count > 0 && target.count > 0
            onClicked:      { }
            debug: true	
        }
    }
}

