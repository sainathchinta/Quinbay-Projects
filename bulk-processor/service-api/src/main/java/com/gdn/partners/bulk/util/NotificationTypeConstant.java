package com.gdn.partners.bulk.util;

import java.util.Map;

import com.gdn.mta.notification.enumeration.NotificationType;
import com.google.common.collect.ImmutableMap;

public interface NotificationTypeConstant {
  String VENDOR_BULK_DOWNLOADED_V2 = "vendor_bulk_downloaded_v2";
  String BULK_DOWNLOADED_V2 = "bulk_downloaded_v2";
  String VENDOR_BULK_DOWNLOADED = "vendor_bulk_downloaded";
  String BULK_DOWNLOADED = "bulk_downloaded";
  String SEND_FOR_NEED_CORRECTION_FOR_PRODUCT_LIMITS = "send_for_need_correction_for_product_limits";
  String DOWNLOAD_DELETED_NEED_CORRECTION_PRODUCTS = "download_deleted_need_correction_products";
  Map<String, Map<Boolean, String>> NOTIFICATION_TYPE_MAP = ImmutableMap.of(VENDOR_BULK_DOWNLOADED,
      ImmutableMap.of(false, VENDOR_BULK_DOWNLOADED, true, VENDOR_BULK_DOWNLOADED_V2), BULK_DOWNLOADED,
      ImmutableMap.of(false, BULK_DOWNLOADED, true, BULK_DOWNLOADED_V2));
}