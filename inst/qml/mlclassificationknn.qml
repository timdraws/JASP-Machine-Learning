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
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList {
            id: target
            name: "target"
            title: qsTr("Target")
            singleVariable: true
            allowedColumns: ["ordinal", "nominal", "nominalText"]
        }
        AssignedVariablesList {
            id: predictors
            name: "predictors"
            title: qsTr("Predictors")
            singleVariable: false
            allowedColumns: ["scale", "ordinal", "nominal"]
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
    }
    
    GroupBox {
        title: qsTr("Plots")

        CheckBox { 
            text: qsTr("Data split") 
            name: "dataSplitPlot"
            checked: true
        }
        
        CheckBox { 
            text: qsTr("Classification accuracy") 
            name: "plotErrorVsK"
            enabled: !optimizationManual.checked 
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
        title: qsTr("Training Parameters")

          ColumnLayout {
        
            GroupBox {
                title: qsTr("Algorithmic Settings")

                IntegerField { 
                    name: "noOfNearestNeighbours"
                    text: qsTr("Nearest neighbors:")
                    defaultValue: 3
                    min: 1
                    max: 999999
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

                    DoubleField  { 
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
                    text: qsTr("Manual")                     
                    name: "optimizationManual"
                    id: optimizationManual 
                }
              
                RadioButton { 
                    text: qsTr("Validation accuracy")
                    name: "optimizationError"
                    checked: true 
                }
            }

            RadioButtonGroup {
                title: qsTr("Cross-Validation")
                name: "modelValid"

                RadioButton { 
                    text: qsTr("None") 
                    name: "validationManual"
                    id: validationManual 
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

                RadioButton { 
                    text: qsTr("Leave-one-out")                 
                    name: "validationLeaveOneOut"
                    id: validationLeaveOneOut 
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
