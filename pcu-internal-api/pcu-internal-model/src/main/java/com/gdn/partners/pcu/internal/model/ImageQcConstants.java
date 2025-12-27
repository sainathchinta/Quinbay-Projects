package com.gdn.partners.pcu.internal.model;

public interface ImageQcConstants {
  String GOOD = "Good";
  String BLUR_PREDICTION = "Blur";
  String PORNOGRAPHY_PREDICTION = "Pornography";
  String TEXT_PREDICTION = "Text";
  String WATERMARK_PREDICTION = "Watermark";

  String GOOD_EN = "No Detection";
  String GOOD_IN = "Belum ada Deteksi";

  String BRAND_EN = "Brand mismatch";
  String BRAND_IN = "Brand tidak cocok";

  String CATEGORY_EN = "Category mismatch";
  String CATEGORY_IN = "Kategori tidak cocok";

  String IMAGE_BASE_PATH = "/filestore/mta/images/source";
}