package com.gdn.mta.product.util;

public final class ProductWorkflowLookup {

  public static final int STATE_DRAFT = 0;
  public static final int STATE_REVIEW_CONTENT = 1;
  public static final int STATE_REVIEW_IMAGE = 2;
  public static final int STATE_ACTIVE = 3;
  public static final int STATE_DELETE = 4;
  public static final int STATE_EDIT = 5;
  public static final int STATE_PROCESS_IMAGE = 6;
  public static final int STATE_MIGRATE = 7;
  public static final int STATE_AUTO_APPROVE = 8;

  public static final String STATE_DRAFT_DESCRIPTION = "Draft";
  public static final String STATE_REVIEW_CONTENT_DESCRIPTION = "Review Konten";
  public static final String STATE_APPROVED_CONTENT_DESCRIPTION = "Setujui Konten";
  public static final String STATE_REVIEW_IMAGE_DESCRIPTION = "Review Gambar";
  public static final String STATE_APPROVED_IMAGE_DESCRIPTION = "Setujui Gambar";
  public static final String STATE_ACTIVE_DESCRIPTION = "Aktif";
  public static final String STATE_DELETE_DESCRIPTION = "Dihapus";
  public static final String STATE_EDIT_DESCRIPTION = "Diubah";
  public static final String STATE_PROCESS_IMAGE_DESCRIPTION = "Gambar Diproses";
  public static final String STATE_PRODUCT_FORCE_REVIEW_DESCRIPTION = "Produk dikirim ke force review";
  public static final String STATE_MIGRATED_PRODUCT = "New product code created";
  public static final String STATE_AUTO_APPROVE_DESCRIPTION = "Auto Approved";

  public static final String STATE_CREATE_TO_DRAFT_NOTES = "Produk {name} ditambahkan. Menunggu proses screening";
  public static final String STATE_DRAFT_TO_REVIEW_CONTENT_NOTES = "Produk diterima. Menunggu review konten";
  public static final String STATE_DRAFT_TO_REVIEW_IMAGE_NOTES = "Produk diterima. Menunggu review gambar";
  public static final String STATE_APPROVED_CONTENT_NOTES = "Konten diterima";
  public static final String STATE_APPROVED_IMAGE_NOTES = "Gambar diterima";
  public static final String STATE_ACTIVE_NOTES = "Status produk menjadi aktif";
  public static final String STATE_EDIT_NOTES = "Produk diubah";
  public static final String STATE_APPROVE_PROCESS_IMAGE_NOTES = "Gambar berhasil diproses";
  public static final String STATE_REJECT_PROCESS_IMAGE_NOTES = "Gambar gagal diproses";
  public static final String STATE_CONTENT_AUTO_APPROVE_NOTES ="Content auto-approved by system.";
  
}
