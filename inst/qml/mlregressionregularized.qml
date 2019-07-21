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
        AssignedVariablesList  { 
            name: "weights"    
            title: qsTr("Weights")   
            singleVariable: true
            allowedColumns: ["scale"] 
        }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox {
            text: qsTr("Evaluation metrics")
            name: "validationMeasures"
        }  

        CheckBox { 
            name: "coefTable"
            text: qsTr("Regression coefficients")
        }
    }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { 
            name: "predictedPerformancePlot"
            text: qsTr("Predictive performance") 
        }

        CheckBox { 
            name: "variableTrace"    
            text: qsTr("Variable trace")

            CheckBox { 
                name: "variableTraceLegend"
                text: qsTr("Legend")
                checked: true          
            }
        }

        CheckBox { 
            name: "lambdaEvaluation"
            text: qsTr("\u03BB evaluation")

            CheckBox { 
                name: "lambdaEvaluationLegend"
                text: qsTr("Legend")
                checked: true      
            }
        }
    }

    Section {
        text: qsTr("Training Parameters")

        RadioButtonGroup {
            title: qsTr("Model Optimization")
            name: "shrinkage"

            RadioButton { 
                text: qsTr("Cross-validated mean squared error")               
                name: "optMin"
                checked: true             
            }

            RadioButton { 
                text: qsTr("Largest \u03BB within 1 SE of min.")
                name: "opt1SE"                            
            }

            RadioButton { 
                text: qsTr("Manual")                       
                name: "manual"  
                childrenOnSameRow: true

                DoubleField { 
                    name: "lambda"
                    defaultValue: 1 
                    min: 0
                    max: 999999
                    fieldWidth: 60              
                }
            }
        }

        GroupBox {

            DoubleField { 
                name: "thresh"     
                text: qsTr("Convergence threshold:")    
                defaultValue: 1e-7
                min: 1e-999
                max: 1
                fieldWidth: 60
                visible: false              
            }

            PercentField { 
                name: "trainingDataManual"  
                text: qsTr("Data used for training:")   
                defaultValue: 80   
                min: 5
                max: 95                                                                   
            }

            DropDown {
                id: penalty
                name: "penalty"
                indexDefaultValue: 0
                label: qsTr("Penalty:")
                values:
                [
                    { label: "Ridge", value: "ridge"},
                    { label: "Lasso", value: "lasso"},
                    { label: "Elastic net", value: "elasticNet"}
                ]
            }

            DoubleField { 
                name: "alpha"      
                text: qsTr("Elastic net parameter (α):")
                defaultValue: 0.5 
                min: 0     
                max: 1
                visible: penalty.currentIndex == 2 
            }

            CheckBox { 
                name: "intercept"  
                text: qsTr("Fit intercept")             
                checked: true                                                                        
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
    //     text: qsTr("Predictions")
    //     debug: true

    //     RadioButtonGroup
    //     {
    //         name: "applyModel"
    //         RadioButton { value: "noApp"         ; text: qsTr("Do not predict data"); checked: true        }
    //         RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target")  }
    //         RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to apply indicator"); id: applyIndicator       }
    //     }

    //     VariablesForm {
    //         visible: applyIndicator.checked
    //         height: 150
    //         AvailableVariablesList { name: "predictionVariables"; allowedColumns: ["nominal"] }
    //         AssignedVariablesList {
    //             name: "indicator"
    //             title: qsTr("Apply indicator")
    //             singleVariable: true
    //             allowedColumns: ["nominal"]
    //         }
    //     }
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
            enabled: 		predictors.count > 1 && target.count > 0
            onClicked:      
            {

             }
            debug: true	
        }
    }

}
