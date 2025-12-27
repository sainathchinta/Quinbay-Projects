package com.gdn.x.mta.distributiontask.model;

import org.apache.commons.lang3.StringUtils;

public enum ErrorCategory {
  VALIDATION("VALIDATION", "Can not process invalid input data "),
  EMPTY_ERROR_MESSAGE(StringUtils.EMPTY, StringUtils.EMPTY),
  SAME_IPR_STATE(StringUtils.EMPTY, Constants.IN_SAME_STATE),
  IPR_ACTION_MUST_NOT_BE_EMPTY("IPR_ERROR_00001", "Silakan pilih tindakan yang ada di daftar."),
  PRODUCT_WAITING_TO_GET_ACTIVATED("IPR_ERROR_00002", "Produk nonaktif. Produk akan ditambahkan ke review IPR setelah diaktifkan."),
  PRODUCT_WHITELISTED("IPR_ERROR_00003", "Produk sudah di-whitelist"),
  INVALID_ACTION_PROVIDED("IPR_ERROR_00004", "Tindakan tidak valid."),
  PRODUCT_EVIDENCE_REQUESTED("IPR_ERROR_00005", "Produk tidak bisa dirilis/review karena bukti telah diminta."),
  PRODUCT_EVIDENCE_SUBMITTED("IPR_ERROR_00006", "Produk tidak bisa di-review karena bukti telah dikirim."),
  PRODUCT_SKU_NOT_FOUND("IPR_ERROR_00007", "SKU produk tidak ditemukan."),
  PRODUCT_SUSPENDED_FROM_INTERNAL("IPR_ERROR_00008", "Produk tidak bisa dirilis/review karena telah di-suspend."),
  PRODUCT_REJECTED_FROM_INTERNAL("IPR_ERROR_00009", "Produk sudah ditolak."),
  NOTES_MUST_NOT_BE_EMPTY("IPR_ERROR_00010", "Berikan catatan sebelum Anda melakukan suspend/whitelist/ajukan bukti untuk produk."),
  REASONS_MUST_NOT_BE_EMPTY("IPR_ERROR_00011", "Berikan alasan suspend sebelum Anda melakukan suspend/ajukan bukti untuk produk."),
  VIOLATION_TYPE_MUST_NOT_BE_EMPTY("IPR_ERROR_00012", "Silakan pilih tipe pelanggaran yang ada di daftar."),
  USER_NOT_ALLOWED_TO_REVIEW("IPR_ERROR_00013", "Anda tidak memiliki akses review."),
  IPR_PRODUCT_IS_ALREADY_UNASSIGNED("IPR_ERROR_00014", "Produk telah dibatalkan sebelumnya."),
  INVALID_STATE_TO_UPDATE_ASSIGNEE("IPR_ERROR_00015", "Gagal memperbarui penerima tugas."),
  PRODUCT_SKU_MUST_NOT_BE_EMPTY("IPR_ERROR_00016", "SKU produk wajib diisi."),
  PRODUCT_IS_PRELIVE("IPR_ERROR_000017", "Prelive SKU cannot be added to IPR");


  private final String message;
  private final String code;

  ErrorCategory(String code, String message) {
    this.code = code;
    this.message = message;
  }

  public String getCode() {
    return this.code;
  }

  public String getMessage() {
    return this.message;
  }
}
