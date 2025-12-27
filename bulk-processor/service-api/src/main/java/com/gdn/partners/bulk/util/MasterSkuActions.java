package com.gdn.partners.bulk.util;

public enum MasterSkuActions {

  ADD_TO_CLUSTER("Tambah ke master SKU"), REMOVE_FROM_CLUSTER("Hapus dari master SKU"), DISMANTLE_CLUSTER(
      "Pisahkan master SKU"), NO_ACTION("No Action");
  private String value;

  MasterSkuActions(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static MasterSkuActions fromValue(String value) {
    for (MasterSkuActions action : MasterSkuActions.values()) {
      if (value.equals(action.getValue())) {
        return action;
      }
    }
    return MasterSkuActions.NO_ACTION;
  }
}
