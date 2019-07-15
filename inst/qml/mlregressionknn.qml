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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {

    VariablesForm {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList {
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
            singleVariable: false
            allowedColumns: ["scale", "nominal", "ordinal"] 
        }
    }
    
    GroupBox {
        title: qsTr("Plots")
        
        CheckBox { 
            text: qsTr("Predicted performance") 
            name: "predictedPerformancePlot"
            checked: true 
        }

        CheckBox { 
            text: qsTr("Mean squared error") 
            name: "plotErrorVsK"
            enabled: !optimizationManual.checked 
        }
    }
    
    Section {
        title: qsTr("Training Parameters")

        ColumnLayout{

            RadioButtonGroup {
                title: qsTr("Model Optimization")
                name: "modelOpt"
                
                RadioButton { 
                    text: qsTr("Mean squared error")          
                    name: "optimizationError"
                    checked: true 
                }

                RadioButton { 
                    id: optimizationManual 
                    text: qsTr("Manual")                        
                    name: "optimizationManual"
                }
            }

            RadioButtonGroup {
                title: qsTr("Cross-Validation")
                name: "modelValid"
                
                RadioButton { 
                    id: validationLeaveOneOut 
                    text: qsTr("Leave-one-out")        
                    name: "validationLeaveOneOut"
                }

                RadioButton { 
                    name: "validationKFold"
                    childrenOnSameRow: true

                    IntegerField {
                        name: "noOfFolds"
                        afterLabel: qsTr("-fold")
                        defaultValue: 3
                        min: 2
                        max: 15
                        fieldWidth: 25
                    }
                }

                RadioButton { 
                    id: validationManual 
                    text: qsTr("None")                       
                    name: "validationManual"
                    checked: true
                }
            }
        }
    
        GroupBox {

            IntegerField { 
                name: "noOfNearestNeighbours"
                text: qsTr("No. of nearest neighbors:") 
                defaultValue: 3 
                min: 1; max: 999999
                fieldWidth: 60
                enabled: optimizationManual.checked 
            }

            IntegerField { 
                name: "maxK"
                text: qsTr("Max. nearest neighbors:") 
                defaultValue: 10 
                min: 1
                max: 999999
                fieldWidth: 60
                enabled: !optimizationManual.checked 
            }

            PercentField { 
                name: "trainingDataManual"
                text: qsTr("Data used for training:")       
                defaultValue: 80
                enabled: validationManual.checked 
            }

            DropDown {
                name: "distanceParameterManual"
                indexDefaultValue: 0
                label: qsTr("Distance:")
                values:
                [
                    { label: "Euclidian", value: "2"},
                    { label: "Manhattan", value: "1"}
                ]
            }
            
            DropDown {
                name: "weights"
                indexDefaultValue: 0
                label: qsTr("Weights:")
                values:
                [
                    { label: "Rectangular", value: "rectangular"},
                    { label: "Epanechnikov", value: "epanechnikov"},
                    { label: "Biweight", value: "biweight"},
                    { label: "Triweight", value: "triweight"},
                    { label: "Cosine", value: "cos"},
                    { label: "Inverse", value: "inv"},
                    { label: "Gaussian", value: "gaussian"},
                    { label: "Rank", value: "rank"},
                    { label: "Optimal", value: "optimal"}
                ]
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
            enabled: 		predictors.count > 0 && target.count > 0
            onClicked:      
            {
                
             }
            debug: true	
        }
    }
}
