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
            name: "classBoostRelInfTable"
            text: qsTr("Relative influence")            
        }
    }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { 
            text: qsTr("Data split") 
            name: "dataSplitPlot"
            checked: true
        }

        CheckBox { 
            name: "plotOOBChangeDev"
            text: qsTr("Out-of-bag improvement")      
        }

        CheckBox { 
            text: qsTr("Predictive performance") 
            name: "predictedPerformancePlot"
        }

        CheckBox { 
            name: "plotDeviance"
            text: qsTr("Deviance")             
        }

        CheckBox { 
            name: "plotRelInf"
            text: qsTr("Relative influence")   
        }
    }

    Section {
        title: qsTr("Training Parameters")

        ColumnLayout {

            GroupBox {
                title: qsTr("Algorithmic Settings")

                IntegerField { 
                    name: "noOfTrees"
                    text: qsTr("Trees:") 
                    defaultValue: 100 
                    min: 1
                    max: 999999
                    fieldWidth: 60
                    enabled: optimizationManual.checked 
                }

                IntegerField { 
                    name: "maxTrees"
                    text: qsTr("Max. trees:") 
                    defaultValue: 100 
                    min: 1
                    max: 999999
                    fieldWidth: 60
                    enabled: !optimizationManual.checked 
                }

                DoubleField  { 
                    name: "shrinkage"
                    text: qsTr("Shrinkage:")                    
                    defaultValue: 0.1 
                    min: 0
                    max: 1     
                    fieldWidth: 60 
                }

                IntegerField { 
                    name: "intDepth" 
                    text: qsTr("Interaction depth:")            
                    defaultValue: 1   
                    min: 1
                    max: 99    
                    fieldWidth: 60 
                }

                IntegerField { 
                    name: "nNode"    
                    text: qsTr("Min. observations in node:")
                    defaultValue: 10  
                    min: 1
                    max: 999999
                    fieldWidth: 60 
                }

                PercentField { 
                    name: "bagFrac"  
                    text: qsTr("Training data used per tree:")  
                    defaultValue: 50                                        
                }

                DropDown {
                    name: "distance"
                    indexDefaultValue: 0
                    label: qsTr("Loss function:")
                    
                    values:
                    [
                        { label: "Gaussian", value: "gaussian"},
                        { label: "Laplace", value: "laplace"},
                        { label: "t", value: "tdist"}
                    ]
                }
            }

            Divider { }

            GroupBox {
                title: qsTr("Data Split Preferences")

                CheckBox {
                    id: testSetIndicator
                    name: "testSetIndicator"
                    label: qsTr("Test set indicator:")
                    childrenOnSameRow: true

                    DropDown {
                        name: "testSetIndicatorVariable"
                        showVariableTypeIcon: true
                        addEmptyValue: true
                        placeholderText: qsTr("None")
                    }
                }

                PercentField { 
                    name: "trainingDataManual"
                    text: qsTr("Data used for training:")
                    defaultValue: 80
                    min: 5
                    max: 95 
                    enabled: !testSetIndicator.checked
                }

                PercentField { 
                    name: "validationDataManual"
                    text: qsTr("Training data used for validation:")
                    defaultValue: 20
                    enabled: validationManual.checked 
                    min: 5
                    max: 95 
                }

                CheckBox { 
                    text: qsTr("Scale predictors") 
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

        ColumnLayout{

            RadioButtonGroup {
                title: qsTr("Model Optimization")
                name: "modelOpt"

                RadioButton { 
                    id: optimizationManual
                    text: qsTr("Manual")                    
                    name: "optimizationManual" 
                }

                RadioButton { 
                    text: qsTr("Out-of-bag mean squared error")             
                    name: "optimizationError"
                    checked: true                  
                }
            }

            RadioButtonGroup {
                title: qsTr("Cross-Validation")
                name: "modelValid"

                RadioButton { 
                    id: validationManual
                    text: qsTr("None")                    
                    name: "validationManual" 
                    checked: true
                }

                RowLayout {
                    spacing: 0
                    
                    RadioButton { 
                        id: validationKFold
                        name: "validationKFold"
                        childrenOnSameRow: true
                        text: qsTr("K-fold")
                    }

                    IntegerField {
                        name: "noOfFolds"
                        afterLabel: qsTr("folds")
                        label: qsTr("with")
                        defaultValue: 5
                        min: 2
                        max: 999
                        fieldWidth: 30
                        visible: validationKFold.checked
                    } 
                }
            }
        }
    }

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
