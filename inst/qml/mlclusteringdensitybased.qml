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
    AvailableVariablesList {name: "variables"}
    AssignedVariablesList {
        name: "predictors"
        title: qsTr("Variables")
        singleVariable: false
        allowedColumns: ["ordinal", "scale"]
    }
}

GroupBox {
    title: qsTr("Tables")

    CheckBox { text: qsTr("Cluster information") ; name: "tableClusterInformation" ; enabled: true ; id: clusterInfo; checked: true
    CheckBox { text: qsTr("Between sum of squares") ; name: "tableClusterInfoBetweenSumSquares" ; checked: false }
    CheckBox { text: qsTr("Total sum of squares") ; name: "tableClusterInfoTotalSumSquares" ; checked: false }
    }
}

GroupBox {
    title: qsTr("Plots")
    CheckBox { text: qsTr("Cluster plot")       ; name: "plot2dCluster" ; checked: false; enabled: true}
    CheckBox { text: qsTr("K-dist plot")         ; name: "k-distplot"    ; checked: false; enabled: true}
}

Section {
    title: qsTr("Training parameters")

    GridLayout {
    RadioButtonGroup {
        title: qsTr("Model optimization")
        name: "modelOpt"
        RadioButton { text: qsTr("k-distance")                             ; name: "k-distance" }
        RadioButton { text: qsTr("Manual")                          ; name: "validationManual"; id: validationManual }
    }

    GroupBox {
        DoubleField { name: "eps"; text: qsTr("eps:") ; decimals: 2; defaultValue: 2 ; min: -1; max: 999999; fieldWidth: 60; enabled: validationManual.checked }
        IntegerField { name: "minPts"; text: qsTr("minPts:") ; defaultValue: 5 ; min: 1; max: 999999; fieldWidth: 60 }
        ComboBox { name: "distance"; label: qsTr("Distance metric:");
            model: ListModel {
                ListElement { key: "Normal densities"            ; value: "Normal densities" }
                ListElement { key: "Correlated densities"                    ; value: "Correlated densities" }
            }
        }
        CheckBox { text: qsTr("Scale variables") ; name: "scaleEqualSD"; checked: true}
        CheckBox { name: "seedBox"; text: qsTr("Set seed:"); childrenOnSameRow: true
            DoubleField  { name: "seed"; defaultValue: 1; min: -999999; max: 999999; fieldWidth: 60 }
        }
    }
}
}

    Section {
    text: qsTr("Predictions")
    debug: true

        RadioButtonGroup
        {
            name: "applyModel"
            RadioButton { value: "noApp"         ; text: qsTr("Do not predict data"); checked: true        }
            RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target")  }
            RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to apply indicator"); id: applyIndicator       }
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

}
