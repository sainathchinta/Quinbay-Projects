package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.google.common.collect.ImmutableList;

public interface BulkMasterSkuUploadParameters {
  String FIRST_MASTER_SKU = "Kode master SKU 1";
  String FIRST_MASTER_SKU_NAME = "Nama master SKU 1 (opsional)";
  String SECOND_MASTER_SKU = "Kode master SKU 2";
  String SECOND_MASTER_SKU_NAME = "Nama master SKU 2 (opsional)";
  String ASSIGNEE = "Pengguna";
  String ANCHOR_SKU = "SKU item / Kode master SKU 1";
  String ANCHOR_SKU_NAME = "Nama SKU (opsional)";
  String REVIEW_ACTION = "Tindakan";
  String ADD_TO_CLUSTER = "Tambah ke master SKU";
  String REMOVE_FROM_CLUSTER = "Hapus dari master SKU";
  String DISMANTLE_CLUSTER = "Pisahkan master SKU";

  Collection<String> MASTER_SKU_BULK_ASSIGNEE =
      Arrays.asList(FIRST_MASTER_SKU, FIRST_MASTER_SKU_NAME, SECOND_MASTER_SKU, SECOND_MASTER_SKU_NAME, ASSIGNEE);

  List<String> MASTER_SKU_BULK_REVIEW_HEADER_LIST =
      ImmutableList.of(ANCHOR_SKU, ANCHOR_SKU_NAME, SECOND_MASTER_SKU, SECOND_MASTER_SKU_NAME, REVIEW_ACTION);

  List<String> MASTER_SKU_REVIEW_ACTIONS_LIST =
      ImmutableList.of(ADD_TO_CLUSTER, REMOVE_FROM_CLUSTER, DISMANTLE_CLUSTER);
}
