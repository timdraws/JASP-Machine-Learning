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
            allowedColumns: ["nominal", "ordinal", "nominalText"] 
        }
        AssignedVariablesList { 
            id: predictors
            name: "predictors"
            title: qsTr("Predictors")
            allowedColumns: ["nominal", "ordinal", "nominalText", "scale"]              
        }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { 
            text: qsTr("Confusion matrix") 
            name: "confusionTable"
            checked: true

            CheckBox { 
                text: qsTr("Display proportions")
                name: "confusionProportions"
            } 
        }   

        CheckBox {
            text: qsTr("Class proportions")
            name: "classProportionsTable"
        }  

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
            text: qsTr("Data split") 
            name: "dataSplitPlot"
            checked: true
        }

        CheckBox { 
            name: "plotTreesVsModelError"
            text: qsTr("Out-of-bag accuracy")        
        }

        CheckBox { 
            name: "rocCurve"
            text: qsTr("ROC curves") 
        }

        CheckBox { 
            name: "andrewsCurve"
            text: qsTr("Andrews curves") 
        }

        CheckBox { 
            name: "plotDecreaseAccuracy"
            text: qsTr("Mean decrease in accuracy") 
        }

        CheckBox { 
            name: "plotIncreasePurity"
            text: qsTr("Total increase in node purity") 
        }

        CheckBox { 
            name: "decisionBoundary"
            text: qsTr("Decision boundary matrix")

            RowLayout {

                CheckBox {
                    name: "plotLegend"
                    text: qsTr("Legend")
                    checked: true 
                }

                CheckBox {
                    name: "plotPoints"
                    text: qsTr("Points")
                    checked: true 
                }
            }
        }
    }

    Section {
        title: qsTr("Data Split Preferences")

        RadioButtonGroup {
            title: qsTr("Holdout Test Data")
            name: "holdoutData"

            RadioButton {
                id: holdoutManual
                name: "holdoutManual"
                childrenOnSameRow: true
                text: qsTr("Sample")

                RowLayout {
                
                    PercentField {    
                        name: "testDataManual"
                        defaultValue: 20
                        min: 5
                        max: 95 
                    }

                    Text {
                        text: qsTr("of all data")
                        enabled: true
                    }
                }
            }

            CheckBox { 
                id: addIndicator  
                name: "addIndicator"
                text: qsTr("Add generated indicator to data")
                Layout.leftMargin: 20
                enabled: holdoutManual.checked

                ComputedColumnField { 
                    name: 		"testIndicatorColumn"
                    text: 		"Name: "
                    fieldWidth: 120
                    visible:    addIndicator.checked
                }
            }

            RadioButton {
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
        }

        RadioButtonGroup {
            title: qsTr("Training and Validation Data")
            name: "modelValid"

            RadioButton {
                name: "validationManual"
                childrenOnSameRow: true
                checked: true
                text: qsTr("Sample")

                RowLayout {

                    PercentField {     
                        name: "validationDataManual"
                        defaultValue: 20
                        min: 5
                        max: 95
                    }

                    Text {
                        text: qsTr("for validation data")
                        enabled: true
                    }
                }
            }
        }
    }

    Section {
        title: qsTr("Training Parameters")

        GroupBox {
            title: qsTr("Algorithmic Settings")

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
                    label: qsTr("Predictors per split:")
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

            CheckBox { 
                text: qsTr("Scale predictors") 
                name: "scaleEqualSD"
                checked: true
            }

            CheckBox { 
                name: "seedBox"
                text: qsTr("Set seed:")
                childrenOnSameRow: true

                DoubleField { 
                    name: "seed"
                    defaultValue: 1
                    min: -999999
                    max: 999999
                    fieldWidth: 60 
                }
            }
        }

        RadioButtonGroup {
            title: qsTr("Number of Trees")
            name: "modelOpt"

            RadioButton { 
                text: qsTr("Fixed")                     
                name: "optimizationManual" 

                IntegerField { 
                    name: "noOfTrees"
                    text: qsTr("Trees:")
                    defaultValue: 100
                    min: 1
                    max: 999999
                    fieldWidth: 60
                }
            }
            
            RadioButton { 
                text: qsTr("Optimized")
                name: "optimizationError"
                checked: true 

                IntegerField { 
                    name: "maxTrees"
                    text: qsTr("Max. trees:") 
                    defaultValue: 100
                    min: 1
                    max: 999999
                    fieldWidth: 60
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
