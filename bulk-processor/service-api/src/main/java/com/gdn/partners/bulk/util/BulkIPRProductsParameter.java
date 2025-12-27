package com.gdn.partners.bulk.util;

import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductSourceIPR;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.google.common.collect.ImmutableMap;

import java.util.Arrays;
import java.util.Collection;

public interface BulkIPRProductsParameter {
  String PRODUCT_NAME = "Nama produk (Opsional)";
  String PRODUCT_SKU = "SKU produk";
  String ACTION = "Tindakan";
  String REASON = "Alasan (Bukti diminta/ Suspend)";
  String SELLER_NOTES = "Catatan untuk seller";
  String REVIEWER_NOTES = "Catatan untuk reviewer";
  String VIOLATION_TYPE = "Tipe pelanggaran";
  String SOURCE = "Sumber";
  String ASSIGNEE = "Reviewer";
  String REVIEW = "Review";
  String RELEASE = "Rilis";
  String WHITELIST = "Whitelist";
  String REQUEST_EVIDENCE = "Ajukan bukti";
  String SUSPEND = "Suspend";
  String CUSTOMER_REPORT = "Laporan pelanggan";
  String BRAND_REPORT = "Laporan brand";
  String RANDOM_SAMPLE = "Sampel acak";
  String IPR_REVIEWERS = "IPR_REVIEWERS";
  String REPORT_DATE = "Tanggal Laporan";
  String REPORTER = "Pelapor";
  String REPORTER_NAME = "Nama Pelapor";
  String REPORTER_PHONE_NUMBER = "Nomor Telepon Pelapor";
  String REPORTER_EMAIL = "Email Pelapor";
  String REPORTER_ADDRESS = "Alamat Pelapor";
  String REPORTER_REASON = "Alasan Takedown";

  Collection<String> BULK_ADD_REVIEW_IPR_PRODUCTS_HEADERS =
      Arrays.asList(PRODUCT_SKU, PRODUCT_NAME, ACTION, REASON, SELLER_NOTES, REVIEWER_NOTES,
          VIOLATION_TYPE, SOURCE, ASSIGNEE, REPORT_DATE, REPORTER, REPORTER_NAME,
          REPORTER_PHONE_NUMBER, REPORTER_EMAIL, REPORTER_ADDRESS, REPORTER_REASON);


  ImmutableMap<String, String> BULK_IPR_ACTIONS_MAP =
      new ImmutableMap.Builder<String, String>()
          .put(REVIEW, ProductStateIPR.IN_REVIEW.name())
          .put(WHITELIST, ProductStateIPR.WHITELISTED.name())
          .put(SUSPEND, ProductStateIPR.SUSPENDED.name())
          .put(REQUEST_EVIDENCE, ProductStateIPR.EVIDENCE_REQUESTED.name())
          .put(RELEASE, ProductStateIPR.RELEASED.name())
          .build();

  ImmutableMap<String, String> BULK_IPR_SOURCE_MAP =
      new ImmutableMap.Builder<String, String>()
          .put(CUSTOMER_REPORT, ProductSourceIPR.CUSTOMER_REPORT.name())
          .put(BRAND_REPORT, ProductSourceIPR.BRAND_REPORT.name())
          .put(RANDOM_SAMPLE, ProductSourceIPR.RANDOM_SAMPLE.name())
          .build();

}
