//
// Copyright (C) 2013-2018 University of Amsterdam
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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP.Theme		1.0

Form {

    VariablesForm {
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

        CheckBox {
            text: qsTr("Evaluation metrics")
            name: "validationMeasures"
        }  

        CheckBox { 
            name: "tableVariableImportance"
            text: qsTr("Variable importance") 
        }
    }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { 
            name: "plotTreesVsModelError"
            text: qsTr("Out-of-bag error")         
        }

        CheckBox { 
            name: "predictedPerformancePlot"         
            text: qsTr("Predictive performance")	      
        }

        CheckBox { 
            name: "plotDecreaseAccuracy"        
            text: qsTr("Mean decrease in accuracy")     
        }

        CheckBox { 
            name: "plotIncreasePurity"        
            text: qsTr("Total increase in node purity") 
        }

    }

    Section {
        title: qsTr("Training Parameters")

        RadioButtonGroup {
            title: qsTr("Model Optimization")
            name: "modelOpt"

            RadioButton { 
                text: qsTr("Out-of-bag mean squared error")              
                name: "optimizationError" 
                checked: true
            }

            RadioButton { 
                id: optimizationManual
                text: qsTr("Manual")                      
                name: "optimizationManual" 
            }
        }

        GroupBox {

            IntegerField { 
                name: "noOfTrees"
                text: qsTr("No. of trees for training:") 
                defaultValue: 500 
                min: 1
                max: 999999
                fieldWidth: 60
                enabled: optimizationManual.checked 
            }

            IntegerField { 
                name: "maxTrees"
                text: qsTr("Max. no. of trees for training:") 
                defaultValue: 500 
                min: 1
                max: 999999
                fieldWidth: 60
                enabled: !optimizationManual.checked 
            }

            PercentField { 
                name: "bagFrac"       
                text: qsTr("Training data used per tree:")
                defaultValue: 50                                        
            }

            RowLayout {

                DropDown {
                    id: noOfPredictors
                    name: "noOfPredictors"
                    indexDefaultValue: 0
                    label: qsTr("Predictors considered per split:")
                    values:
                    [
                        { label: "Auto", value: "auto"},
                        { label: "Manual", value: "manual"}
                    ]
                } 

                IntegerField  { 
                    name: "numberOfPredictors"
                    defaultValue: 1
                    min: -999999
                    max: 999999
                    visible: noOfPredictors.currentIndex == 1 
                }
            }

            Divider { }

            PercentField { 
                name: "trainingDataManual"     
                text: qsTr("Data used for training:")     
                defaultValue: 80                                        
            }

            CheckBox { 
                text: qsTr("Scale variables") 
                name: "scaleEqualSD"
                checked: true
            }

            CheckBox { 
                name: "seedBox"
                text: qsTr("Set seed:")
                childrenOnSameRow: true
                checked: true

                DoubleField { 
                    name: "seed"
                    defaultValue: 1
                    min: -999999
                    max: 999999
                    fieldWidth: 60 
                }
            }
        }
    }

    // Section {
    //   text: qsTr("Predictions")
    //   debug: true
      
    //       RadioButtonGroup
    //       {
    //           name: "applyModel"
    //           RadioButton { value: "noApp"         ; text: qsTr("Do not predict data"); checked: true        }
    //           RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target")  }
    //           RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to apply indicator"); id: applyIndicator       }
    //       }
      
    //       VariablesForm {
    //       visible: applyIndicator.checked
    //           height: 150
    //           AvailableVariablesList { name: "predictionVariables"; allowedColumns: ["nominal"] }
    //           AssignedVariablesList {
    //                       name: "indicator"
    //                       title: qsTr("Apply indicator")
    //                       singleVariable: true
    //                       allowedColumns: ["nominal"]
    //           }
    //       }  
    // }

    Item {
        height: 			saveModel.height
        Layout.fillWidth: 	true
        Layout.columnSpan: 2

        Button 
        {
            id: 			saveModel
            anchors.right: 	parent.right
            text: 			qsTr("<b>Save Model</b>")
            enabled: 		predictors.count > 1 && target.count > 0
            onClicked:      
            {
                
             }
            debug: true	
        }
    }
}
