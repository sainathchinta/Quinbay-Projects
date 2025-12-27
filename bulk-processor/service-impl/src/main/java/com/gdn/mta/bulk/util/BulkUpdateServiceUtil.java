package com.gdn.mta.bulk.util;

import static com.gdn.partners.bulk.util.Constant.BLIBLI_SKU_HEADER;
import static com.gdn.partners.bulk.util.Constant.BLIBLI_SKU_HEADER_WITH_EXAMPLE;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.B2BFields;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.BulkUpdateChangeType;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.UpsertValidationDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.mta.bulk.dto.inventory.WarehouseInventoryResponseDTO;
import com.gdn.mta.bulk.service.PCBOutboundService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import com.gdn.x.mta.model.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.util.Strings;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkCncUpsertErrorDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.VatUpdateDto;
import com.gdn.mta.bulk.dto.WholeSaleCount;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.BulkUpdateServiceValidation;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.repository.campaign.CampaignRepository;
import com.gdn.mta.bulk.repository.pcb.ProductCategoryBaseRepository;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.NotificationService;
import com.gdn.partners.bulk.util.BulkConfigurationUpdateParameters;
import com.gdn.partners.bulk.util.BulkProductSuspensionParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by virajjasani on 27/07/16.
 */
@Service
@Slf4j
public class BulkUpdateServiceUtil {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkUpdateServiceUtil.class);

  public static final String FAILURE_REASON = "Alasan Kegagalan";
  public static final String SPACE = " ";
  public static final String IS = ": ";
  public static final String AND_SYMBOL = ", ";
  public static final String END_SYMBOL = ". ";
  public static final String FINISH = " selesai";

  public static final String BULK_PROCESS_NULL_ERROR = "No data available in BulkProcess";
  public static final String BUSINESS_PARTNER_NULL_ERROR = "No merchant info found";
  public static final String FILE_BLANK_ERROR = "File tidak boleh kosong";
  public static final String BLIBLI_SKU_BLANK = "Blibli-SKU tidak boleh kosong";
  public static final String BLIBLI_PRODUCT_SKU_BLANK = "Blibli Product SKU tidak boleh kosong";
  public static final String BLIBLI_SKU_PP_CODE_BLANK = "Kode Blibli-SKU dan PP tidak boleh kosong";
  private static final String REGULAR_PRICE_MUST_BE_DECIMAL = "Harga Normal harus desimal";
  private static final String SELLING_PRICE_MUST_BE_DECIMAL = "Harga jual harus desimal";
  private static final String PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE =
      "Harga Normal harus lebih besar dari ";
  private static final String PRICE_MUST_BE_GREATER_THAN_SELLING_PRICE =
      "Harga Normal harus lebih besar dari harga jual";
  private static final String STOCK_MUST_BE_NUMBER = "Stok harus berupa angka";
  private static final String PO_QUOTA_MUST_BE_NUMBER = "PO Quota harus berupa angka";
  private static final String STOCK_MUST_BE_POSITIVE_VALUE = "Stok harus bernilai positif";
  private static final String PO_QUOTA_MUST_BE_POSITIVE_VALUE = "PO Quota harus bernilai positif";
  private static final String PRICE_CUT_MUST_BE_NUMBER = "Price Cut harus berupa angka";
  private static final String MIN_KUOTA_MUST_BE_NUMBER = "Min. Kuota harus berupa angka";
  private static final String PRICE_CUT_CANNOT_BE_ZERO_AND_MUST_BE_POSITIVE_VALUE =
      "Price Cut tidak boleh sama dengan 0 dan harus bernilai positif";
  private static final String MUST_BE_NUMBER = "value harus berupa angka";
  private static final String CANNOT_BE_ZERO_AND_MUST_BE_POSITIVE_VALUE =
      "value tidak boleh sama dengan 0 dan harus bernilai positif";
  private static final String MUST_BE_POSITIVE_VALUE =
      "value harus bernilai positif";
  private static final String FAILED_TO_GET_DATA = "Kode Produk tidak valid";
  private static final String FAILED_DATA_NOT_FOUND = FAILED_TO_GET_DATA + END_SYMBOL + "Produk tidak ditemukan";
  public static final String DUPLICATE_BLIBLI_SKU = "Duplikat Blibli SKU";
  public static final String DUPLICATE_OFFLINE_ITEM = "Duplikat Blibli SKU and Kode Lokasi";
  public static final String DUPLICATE_FBB_PICKUP_POINT = "Duplicate FBB pickupoint present in the sheet";

  private static final String CATEGORY_NOT_INCLUDED_IN_CAMPAIGN =
      "Kode Produk tidak valid. Kategori ini tidak termasuk di dalam campaign";
  private static final String BRANDS_NOT_INCLUDED_IN_CAMPAIGN =
      "Kode Produk tidak valid. Brand ini tidak termasuk di dalam campaign";
  private static final String MERCHANT_CODE_NOT_INCLUDED_IN_CAMPAIGN =
      "Kode Produk tidak valid. Merchant Code ini tidak termasuk di dalam campaign";
  private static final String PRICE_CUT_CANT_BE_GREATER_THAN_SELLING_PRICE =
      "Harga discount tidak boleh melebihi harga jual";
  private static final String PRODUCT_KUOTA_LESS_THAN_MIN_QUOTA = "Produk tidak memenuhi min. kuota ";
  private static final String INPUT_QUOTA_CANT_BE_LESS_THAN_USED_QUOTA =
      "Quota tidak boleh lebih kecil dari quota yang sudah terpakai";

  private static final String PICKUP_POINT_EMPTY = "Pickup point tidak boleh kosong";
  private static final String CNC_STATUS_EMPTY = "CNC status cannot be empty";
  private static final String PICKUP_POINT_INVALID = "Pickup point tidak valid";
  private static final String SELLER_SKU_INVALID = "Seller Sku tidak valid";
  private static final String BUYABLE_INVALID = "Buyable harus bernilai 0 atau 1";
  private static final String OFF_TO_ON_INVALID = "OfflineToOnline harus bernilai 0 atau 1";
  private static final String CNC_STATUS_INVALID = "CNC harus bernilai 0 atau 1";
  private static final String PRODUCT_NAME_MUST_NOT_BE_BLANK = "Product name tidak boleh kosong";
  public static final String HEADER_MISMATCH = "Excel file headers do not match";
  public static final String HEADER_MISMATCH_PRODUCT =
      " Header file excel tidak dapat dibaca. Silakan periksa kembali dan perbaiki file yang diupload.";

  public static final String ARCHIVED_ERROR =
      "Tidak berhasil update  product yang telah diarsipkan. Download excel yang telah terupdate";
  public static final String DEFAULT_CHANNEL_ID = "DEFAULT";

  private static final String INTERNAL_USER_BULK_PROCESS_TYPE = "InternalUserBulkProcess";
  public static final String DATA_NOT_FOUND = "data yang dimasukan tidak ditemukan";
  public static final String PRODUCTS_SUCCESSFULLY_UPDATED = "Produk berhasil diperbarui: ";
  public static final String PRODUCTS_SUCCESSFULLY_SUSPENDED = "Product suspension/activation process completed: ";
  public static final String PRODUCTS_SUCCESSFULLY_INTERNAL_BULK_UPLOAD = "Product internal bulk upload process completed: ";
  public static final String PRODUCTS_NOT_UPDATED = "Produk gagal diperbarui: ";
  public static final String PRODUCTS_NOT_SUSPENEDED = "Failed product suspension action for the products: ";
  public static final String PRODUCTS_NOT_UPDATE = "Failed to update product for the products: ";
  public static final String PRODUCTS_CAMPAIGN_ADD = "Penambahan produk promo ";
  public static final String PRODUCTS_CAMPAIGN_SUCCESSFULLY_ADDED = "Produk berhasil ditambahkan: ";
  public static final String PRODUCTS_CAMPAIGN_NOT_ADDED = "Produk gagal ditambahkan: ";
  public static final String PRODUCTS_INSTANT_PICKUP_PROCESSED =
      "Proses update / insert produk offline : ";
  public static final String PRODUCTS_INSTANT_PICKUP_SUCCESSFULLY_PROCESSED = "Produk berhasil diproses: ";
  public static final String PRODUCTS_INSTANT_PICKUP_FAILED_PROCESSED = "Produk gagal diproses: ";
  public static final String INTERNAL_ERROR =
      "System Internal bermasalah, silahkan coba kembali beberapa saat lagi";
  public static final String PRIVILEGED_MAP_SIZE_ERROR =
      "privileged map for business partner should have size: ";
  public static final int ERROR_COUNT = 100;
  public static final int SELLER_SKU_LIMIT = 255;

  private static final String DESCRIPTION_PRODUCT_BULK_PROCESS = "DESCRIPTION_PRODUCT_BULK_PROCESS";
  private static final String BAHASA = "in";
  private static final Pattern ITEM_SKU_PATTERN = Pattern.compile(Constant.ITEM_SKU_PATTERN);
  private static final Pattern PRODUCT_SKU_PATTERN = Pattern.compile(Constant.PRODUCT_SKU_PATTERN);
  public static final String CONFIGURATION_SUCCESSFULLY_UPLOADED =
      "Merchant/Category configuration process completed: ";
  public static final String CONFIGURATION_NOT_UPLOADED =
      "Failed to upload configuration action for the Merchant/Category: ";
  public static final String SUCCESSFULLY_ARCHIVED_EN = "Product successfully archived";
  public static final String SUCCESSFULLY_UPDATED_EN = "Product successfully updated";
  public static final String VAT_SUCCESSFULLY_UPDATED_EN = "Info VAT for %s product updated";
  public static final String SUCCESSFULLY_ARCHIVED = "Produk berhasil diarsipkan";
  public static final String SUCCESSFULLY_UPDATED = "Produk berhasil diperbarui";
  public static final String VAT_SUCCESSFULLY_UPDATED = "VAT Info untuk %s Produk berhasil";
  public static final String FAILED_ARCHIVED_EN = "Failed archived";
  public static final String FAILED_UPDATE_EN = "Failed update";
  public static final String VAT_FAILED_UPDATE_EN = "%s product failed to be updated";
  public static final String VAT_FAILED_UPDATE = "%s produk gagal diperbarui";
  public static final String FAILED_UPDATE = "Gagal diperbarui";
  public static final String FAILED_ARCHIVED = "Gagal diarsipkan";
  public static final String FAILED_MESSAGE_EN = "Please check your product and try again.";
  public static final String FAILED_MESSAGE = "Silakan cek produk Anda dan coba lagi.";
  public static final String ALL_PRODUCTS_UPDATED_EN = "All products on has been updated";
  public static final String ALL_PRODUCTS_UPDATED_IN = "Semua produk di berhasil diperbarui.";
  public static final String PROMO_PRODUCTS_UPDATED_EN =
      "%d of %d products that updated has been registered on another promo. Make sure you update the right product.";
  public static final String PROMO_PRODUCTS_UPDATED_IN =
      "%d dari %d produk yang diperbarui telah terdaftar di promo lain. "
          + "Pastikan produk yang diperbarui sudah benar. Lihat detail";
  public static final String ALL_PRODUCTS_FAILED_EN =
      "Failed to update all the products. Please change the products";
  public static final String ALL_PRODUCTS_FAILED_IN =
      "Gagal memperbarui semua produk. Silakan ubah produk.";
  public static final String PROMO_UPDATED_WITH_MAX_DISCOUNT_EN =
      "%d of %d products that updated has been registered on another promo and have above 70%% discount";
  public static final String PROMO_UPDATED_WITH_MAX_DISCOUNT_IN =
      "%d dari %d produk yang diperbarui telah terdaftar di promo lain dan memiliki diskon di atas 70%%.";
  public static final String PRODUCT_UPDATED_WITH_MAX_DISCOUNT_EN =
      "%d of %d products that updated have above 70%% discount. Make sure the discount is correct.";
  public static final String PRODUCT_UPDATED_WITH_MAX_DISCOUNT_IN =
      "%d dari %d produk yang diperbarui memiliki diskon di atas 70%%. Pastikan diskon sudah benar.";
  public static final String PARTIAL_UPDATE_EN = "%d out of %d products have been updated. Check the product ";
  public static final String PARTIAL_UPDATE_IN = "%d dari %d berhasil diperbarui. Cek produknya di sini. ";
  public static final String SAME_THRESHOLD =
      "%d products successfully updated. %d products which has whole sale price get updated and other %d is failed";
  public static final String DIFFERENT_THRESHOLD =
      "%d products successfully updated. %d products which has whole sale price get turn off and other %d is failed";
  public static final String BOTH_SAME_AND_DIFFERENT_THRESHOLD =
      "%d products successfully updated. %d products which has whole sale price get turn off, %d others get updated and other %d is failed";
  public static final String SAME_THRESHOLD_IN =
      "%d products successfully updated. %d products which has whole sale price get updated and other %d is failed";
  public static final String DIFFERENT_THRESHOLD_IN =
      "%d products successfully updated. %d products which has whole sale price get turn off and other %d is failed";
  public static final String BOTH_SAME_AND_DIFFERENT_THRESHOLD_IN =
      "%d products successfully updated. %d products which has whole sale price get turn off, %d others get updated and other %d is failed";

  private static final String CLICK_AND_COLLECT_PRODUCTS_SUCCESS_DELETED_IN = "%d produk Click & Collect terhapus. ";
  private static final String CLICK_AND_COLLECT_PRODUCTS_FAILED_DELETED_IN =
      "%d produk Click & Collect gagal dihapus. ";
  private static final String VARIANTS_SUCCESS_DELETED_IN = "%d varian terhapus. ";
  private static final String VARIANTS_FAILED_DELETED_IN =
      "Gagal menghapus %d varian. ";

  private static final String CHECK_FAILED_PRODUCTS_HERE_IN = "Lihat produk yang gagal di sini.";
  private static final String CHECK_YOUR_EXCEL_FILE_IN = "Silakan cek file excel Anda dan coba lagi.";

  private static final int CNC_UPLOAD_HEADERS_SIZE = 5;
  private static final int CNC_DELETE_UPLOAD_HEADERS_SIZE = 2;

  private static final String SELLER_NOT_AUTHORIZED_TO_EDIT = "User does not have access to edit ";
  private static final String SUSPENSION_FAILED_REASON  = "Failure Reasons";
  private static final String BFB_PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE =
    "Bfb Harga Dasar harus lebih besar dari %s.";

  private static final String BFB_MANAGED_INVALID = "Bfb yang dikelola harus 0 atau 1";
  private static final String BFB_PRICE_MUST_BE_NUMBER = "Harga Bfb harus berupa angka";
  private static final String BFB_PRICE_MUST_BE_GREATER_THAN_ZERO = "Harga Bfb harus lebih besar dari 0";
  public static final String PICKUP_POINT_NOT_ACCESSIBLE = "Gagal menambahkan produk karena Anda tidak memiliki akses untuk alamat pengambilan ini.";
  public static final String SCHEDULE_REMOVAL_IN_MESSAGE = "The status has been changed and schedule has been removed for %s. View details: <a href=%s>click here</a>.";;
  public static final String SCHEDULE_REMOVAL_MESSAGE = "Status telah diubah dan penjadwalan telah dihapus untuk produk %s. Lihat detail: <a href=%s>click here</a>.";
  public static final String DEFAULT_STOCK_REMINDER_VALUE = "0";

  @Value("${bulk.process.getProductSummary.batch.size:100}")
  private int GET_BATCH_SIZE;

  @Value("${update.schedules.removal.enabled}")
  private boolean updateScheduleRemovalEnabled;

  @Value("${seller.pdp.url}")
  private String sellerPDPUrl;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;

  @Value("${pre.order.quota.feature.switch}")
  private boolean preOrderQuotaFeatureSwitch;

  @Value("${validate.bulk.archive.headers.enabled}")
  private boolean validateBulkArchiveHeadersEnabled;

  private static final Map<String, String> HEADER_TO_PRIVILEGED_MAP =
      ImmutableMap.<String, String>builder()
          .put(BulkParameters.PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.SELLING_PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.STOCK_HEADER, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.STOCK_REMINDER_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.TYPE_HANDLING_HEADER, BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE)
          .put(BulkParameters.PICKUP_POINT_HEADER, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.OFFLINE_TO_ONLINE_HEADER, BulkParameters.PRIVILEGE_EDIT_O2O)
          .put(BulkParameters.AMPHI_SKU_STATUS, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.EXTERNAL_SKU_STATUS, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.CNC_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_CNC_STATUS)
          .build();

  private static final Map<String, String> HEADER_TO_PRIVILEGED_MAP_CNC_1P =
      ImmutableMap.<String, String>builder()
          .put(BulkParameters.PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.SELLING_PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.STOCK_HEADER, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.STOCK_REMINDER_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.TYPE_HANDLING_HEADER, BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE)
          .put(BulkParameters.PICKUP_POINT_HEADER, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.OFFLINE_TO_ONLINE_HEADER, BulkParameters.PRIVILEGE_EDIT_O2O)
          .put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P,
              BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P,
              BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.CNC_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_CNC_STATUS)
          .put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .build();

  private static final Map<String, String> HEADER_TO_PRIVILEGED_MAP_CNC_1P_UPDATED_INSTORE_HEADER =
      ImmutableMap.<String, String>builder()
          .put(BulkParameters.PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.SELLING_PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.STOCK_HEADER, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.PO_QUOTA, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.STOCK_REMINDER_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.TYPE_HANDLING_HEADER, BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE)
          .put(BulkParameters.PICKUP_POINT_HEADER, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.IN_STORE_HEADER, BulkParameters.PRIVILEGE_EDIT_O2O)
          .put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P,
              BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P,
              BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.CNC_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_CNC_STATUS)
          .put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .build();

  private static final Map<String, String> HEADER_BOOLEAN_VALUES =
      ImmutableMap.<String, String>builder()
          .put(BulkParameters.PURCHASED_HEADER, BUYABLE_INVALID)
          .put(BulkParameters.OFFLINE_TO_ONLINE_HEADER, OFF_TO_ON_INVALID)
          .put(BulkParameters.CNC_STATUS_HEADER, CNC_STATUS_INVALID)
          .put(BulkParameters.BFB_MANAGED, BFB_MANAGED_INVALID)
          .build();

  private static final List<String> REQUIRED_HEADERS =
      Arrays.asList(BulkParameters.BLIBLI_SKU, BulkParameters.BLIBLI_PRODUCT_SKU, BulkParameters.PICKUP_POINT_HEADER,
          BulkParameters.PRICE_HEADER, BulkParameters.SELLING_PRICE_HEADER);

  private static final List<String> EAN_UPLOAD_REQUIRED_HEADERS =
      Arrays.asList(BulkParameters.BLIBLI_PRODUCT_SKU, BulkParameters.PARENT_PRODUCT_NAME, BulkParameters.BLIBLI_SKU,
          BulkParameters.NAMA_PRODUK, BulkParameters.EAN_OR_UPC);

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductCategoryBaseRepository productCategoryBaseRepository;

  @Autowired
  private CampaignRepository campaignRepository;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${pricing.multipickuppoint.enabled}")
  private boolean pricingMultiPickupPointEnabled;

  @Value("${fbb.pickup.point.fetch.size}")
  private int fbbPickupPointFetchSize;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${pricing.campaign.recommendation.enabled}")
  private boolean pricingCampaignRecommendationEnabled;

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Value("${bulk.update.header.validation}")
  private boolean bulkUpdateHeaderValidation;

  @Value("${validate.headers.on.privileges}")
  private boolean validateHeadersOnPrivileges;

  public BusinessPartnerRepository getBusinessPartnerRepository() {
    return businessPartnerRepository;
  }

  public void setBusinessPartnerRepository(BusinessPartnerRepository businessPartnerRepository) {
    this.businessPartnerRepository = businessPartnerRepository;
  }

  @Autowired
  private XProductOutboundService xProductOutboundService;

  public void setxProductOutboundService(XProductOutboundService xProductOutboundService) {
    this.xProductOutboundService = xProductOutboundService;
  }

  public ProductLevel3Repository getProductLevel3Repository() {
    return productLevel3Repository;
  }

  public void setProductLevel3Repository(ProductLevel3Repository productLevel3Repository) {
    this.productLevel3Repository = productLevel3Repository;
  }

  public ProductCategoryBaseRepository getProductCategoryBaseRepository() {
    return productCategoryBaseRepository;
  }

  public void setProductCategoryBaseRepository(
      ProductCategoryBaseRepository productCategoryBaseRepository) {
    this.productCategoryBaseRepository = productCategoryBaseRepository;
  }

  public CampaignRepository getCampaignRepository() {
    return campaignRepository;
  }

  public void setCampaignRepository(CampaignRepository campaignRepository) {
    this.campaignRepository = campaignRepository;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public BulkProcessService getBulkProcessService() {
    return bulkProcessService;
  }

  public void setBulkProcessService(BulkProcessService bulkProcessService) {
    this.bulkProcessService = bulkProcessService;
  }

  public NotificationService getNotificationService() {
    return notificationService;
  }

  public void setNotificationService(NotificationService notificationService) {
    this.notificationService = notificationService;
  }

  public void setPcbOutboundService(PCBOutboundService pcbOutboundService){
    this.pcbOutboundService = pcbOutboundService;
  }



  public BulkAddCampaignProductQueue getBulkAddCampaignProductQueue(String storeId,
      String requestId, String bulkProcessCode, BulkAddCampaignProductDTO bulkAddCampaignProductDTO)
      throws InvocationTargetException, IllegalAccessException {
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue = new BulkAddCampaignProductQueue();
    BeanUtils.copyProperties(bulkAddCampaignProductQueue, bulkAddCampaignProductDTO);
    bulkAddCampaignProductQueue.setStoreId(storeId);
    bulkAddCampaignProductQueue.setRequestId(requestId);
    bulkAddCampaignProductQueue.setBulkProcessCode(bulkProcessCode);
    bulkAddCampaignProductQueue.setFilterOutInactiveCn(bulkAddCampaignProductDTO.isFilterOutInactiveCn());
    return bulkAddCampaignProductQueue;
  }

  public BulkUpdateQueue getBulkUpdateQueue(String storeId, String requestId, String bulkProcessCode,
      BulkUpdateProcessDTO bulkProcessUpdateRequest) {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setFileName(bulkProcessUpdateRequest.getFileName());
    bulkUpdateQueue.setStoreId(storeId);
    bulkUpdateQueue.setBusinessPartnerCode(bulkProcessUpdateRequest.getBusinessPartnerCode());
    bulkUpdateQueue.setBulkProcessType(bulkProcessUpdateRequest.getBulkProcessType());
    bulkUpdateQueue.setBulkProcessCode(bulkProcessCode);
    bulkUpdateQueue.setPrivilegedMap(bulkProcessUpdateRequest.getPrivilegedMap());
    bulkUpdateQueue.setUpdatedBy(bulkProcessUpdateRequest.getUpdatedBy());
    bulkUpdateQueue.setClientHost(bulkProcessUpdateRequest.getClientHost());
    bulkUpdateQueue.setRequestId(requestId);
    return bulkUpdateQueue;
  }

  public BulkProcess getBulkProcess(String storeId, String requestId, String bulkProcessCode,
      BulkUpdateProcessDTO bulkProcessUpdateRequest, int successCount, int errorCount, boolean isTrustedSeller,
      boolean updatePriorityQueueEnabled) throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkUpdate(true);
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setBulkProcessType(bulkProcessUpdateRequest.getBulkProcessType());
    if(updatePriorityQueueEnabled && isTrustedSeller) {
        bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    }
    bulkProcess.setBusinessPartnerCode(bulkProcessUpdateRequest.getBusinessPartnerCode());
    bulkProcess.setBulkProcessCode(bulkProcessCode);
    bulkProcess.setStoreId(storeId);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(requestId);
    bulkProcess.setSuccessCount(successCount);
    bulkProcess.setErrorCount(errorCount);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    return bulkProcess;
  }

  public BulkProcess getBulkProcess(MasterDataBulkUpdateRequest masterDataBulkUpdateRequest,
      int successCount, int errorCount) {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkUpdate(true);
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setBulkProcessType(INTERNAL_USER_BULK_PROCESS_TYPE);
    bulkProcess.setBulkProcessCode(masterDataBulkUpdateRequest.getBulkProcessCode());
    bulkProcess.setStoreId(masterDataBulkUpdateRequest.getStoreId());
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(masterDataBulkUpdateRequest.getRequestId());
    bulkProcess.setSuccessCount(successCount);
    bulkProcess.setErrorCount(errorCount);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    return bulkProcess;
  }

  public BulkInternalProcess getBulkProcess(BulkProductSuspensionRequest bulkProductSuspensionRequest, int successCount,
      int errorCount) {
    BulkInternalProcess bulkInternalProcess =
        BulkInternalProcess.builder().internalProcessRequestCode(bulkProductSuspensionRequest.getBulkProcessCode())
            .processType(BulkInternalProcessType.SUSPEND.name()).fileName(bulkProductSuspensionRequest.getFilePath())
            .startTime(Calendar.getInstance().getTime()).errorCount(errorCount).totalCount(0).successCount(successCount)
            .status(ProcessStatus.PENDING.name()).notes(bulkProductSuspensionRequest.getActionType()).build();
    bulkInternalProcess.setStoreId(bulkProductSuspensionRequest.getStoreId());
    return bulkInternalProcess;
  }

  public BulkProcess getBulkProcess(BulkVendorProductAssignRequest bulkVendorProductAssignRequest, int successCount,
      int errorCount) {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setCreatedBy(
        bulkVendorProductAssignRequest.getUpdatedBy()); //updatedBy is the username itself passed from internal-api
    bulkProcess.setBulkUpdate(true);
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setBulkProcessType(bulkVendorProductAssignRequest.getBulkProcessType());
    bulkProcess.setBulkProcessCode(bulkVendorProductAssignRequest.getBulkProcessCode());
    bulkProcess.setStoreId(bulkVendorProductAssignRequest.getStoreId());
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(bulkVendorProductAssignRequest.getRequestId());
    bulkProcess.setSuccessCount(successCount);
    bulkProcess.setErrorCount(errorCount);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    return bulkProcess;
  }

  public static BulkInternalProcess getBulkProcessFromBulkConfigurationUpdateRequest(
      BulkConfigurationUpdateRequest request, int successCount, int errorCount) {
    BulkInternalProcess bulkInternalProcess =
        BulkInternalProcess.builder().internalProcessRequestCode(request.getBulkProcessCode())
            .processType(BulkInternalProcessType.CONFIGURATION.name()).fileName(request.getFilePath())
            .startTime(Calendar.getInstance().getTime()).errorCount(errorCount).totalCount(0).successCount(successCount)
            .status(ProcessStatus.PENDING.name()).notes(request.getActionType()).build();
    bulkInternalProcess.setStoreId(request.getStoreId());
    return bulkInternalProcess;
  }

  public boolean validateExcelFileForBulkArchive(Sheet worksheet, BulkProcess bulkProcess,
      BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter) throws IOException {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    final String businessPartnerCode = bulkUpdateQueue.getBusinessPartnerCode();
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(worksheet, 0);
    if (headers.size() == 0) {
      LOGGER.error(
          "No data found for bulk archive. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
      updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }
    if (headers.containsValue(BulkParameters.BLIBLI_SKU)) {
      return true;
    }
    return false;
  }

  public boolean validateExcelFileForBulkProductsArchive(Sheet worksheet, BulkProcess bulkProcess,
      BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter) throws IOException {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    final String businessPartnerCode = bulkUpdateQueue.getBusinessPartnerCode();
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(worksheet, 0);
    if (headers.size() == 0) {
      LOGGER.error(
          "No data found for bulk archive. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
      updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }
    if (validateBulkArchiveHeadersEnabled) {
      counter.incrementHeaderValidationCounter();
      return headers.containsValue(BulkParameters.BLIBLI_PRODUCT_SKU)
          && headers.containsValue(BulkParameters.PARENT_PRODUCT_NAME)
          && headers.size() == 2;
    }
    else {
      return headers.containsValue(BulkParameters.BLIBLI_PRODUCT_SKU);
    }
  }

  public boolean validateExcelFileForBulkVatUpdate(Sheet worksheet, BulkProcess bulkProcess,
      BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter) throws IOException {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    final String businessPartnerCode = bulkUpdateQueue.getBusinessPartnerCode();
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(worksheet, 0);
    if (headers.size() == 0) {
      LOGGER.error("No data found for bulk vat update. storeId: {}, bulkProcessCode: {}, " + "businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode);
      updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }
    if (headers.containsValue(BulkParameters.SKU_CODE_HEADER) && headers.containsValue(BulkParameters.SUBJECT_TO_VAT)) {
      return true;
    }
    return false;
  }

  public boolean validateExcelFileForBulkUpdateOff2On(Sheet worksheet, BulkProcess bulkProcess,
      BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter) throws IOException {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    final String businessPartnerCode = bulkUpdateQueue.getBusinessPartnerCode();
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(worksheet, 0);
    if (headers.size() == 0) {
      LOGGER.error(
          "No data found for bulk update off2on. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
      updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }
    return headers.containsValue(BulkParameters.BLIBLI_PRODUCT_SKU) && headers
        .containsValue(BulkParameters.PARENT_PRODUCT_NAME);
  }

  public boolean authorizeUploadBulkUpdate(Map<String, Boolean> privilegedMap, Sheet workSheet,
      BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter,
      MerchantStatusType merchantStatusType, boolean cncForWarehouseFeatureSwitch,
      boolean includePoQuotaHeaderValidation) throws IOException {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    final String businessPartnerCode = bulkUpdateQueue.getBusinessPartnerCode();
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(workSheet, 0);
    Set<String> availableHeaders = new HashSet<>(headers.values());
    return authorizeBulkUpdateHeaders(privilegedMap, bulkProcess, counter, merchantStatusType,
      cncForWarehouseFeatureSwitch, headers, storeId, bulkProcessCode, businessPartnerCode,
      availableHeaders, includePoQuotaHeaderValidation);
  }

  public boolean authorizeBulkUpdateHeaders(Map<String, Boolean> privilegedMap, BulkProcess bulkProcess,
    BulkUpdateErrorCounter counter, MerchantStatusType merchantStatusType,
    boolean cncForWarehouseFeatureSwitch, Map<Integer, String> headers, String storeId,
    String bulkProcessCode, String businessPartnerCode, Set<String> availableHeaders,
      boolean includePoQuotaHeaderValidation) {
    if (headers.size() == 0) {
      LOGGER.error(
          "No data found in file for bulk update. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
      updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }

    if (!checkIfHeadersAreValidForBulkUpdate(headers)) {
      return false;
    }

    for (Iterator<Map.Entry<Integer, String>> it = headers.entrySet().iterator(); it.hasNext(); ) {
      Map.Entry<Integer, String> entry = it.next();
      if (BulkParameters.WAREHOUSE_STOCK_HEADER.equalsIgnoreCase(entry.getValue()) || BulkParameters.IN_STORE_HEADER
          .equalsIgnoreCase(entry.getValue())) {
        it.remove();
      }
    }
    int totalFieldsAuthorized = 3;
    if (headers.containsValue(BulkParameters.SKU_CODE_HEADER)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.BLIBLI_PRODUCT_SKU)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.PARENT_PRODUCT_NAME)) {
      totalFieldsAuthorized++;
    }
    Map<String, String> headerMap;
    if (validateHeadersOnPrivileges) {
      headerMap = HEADER_TO_PRIVILEGED_MAP_CNC_1P_UPDATED_INSTORE_HEADER;
    } else {
      headerMap =
          cncForWarehouseFeatureSwitch ? HEADER_TO_PRIVILEGED_MAP_CNC_1P : HEADER_TO_PRIVILEGED_MAP;
    }
    for (Map.Entry<String, String> mapEntry : headerMap.entrySet()) {
      if (headers.containsValue(mapEntry.getKey())) {
        if (!privilegedMap.getOrDefault(mapEntry.getValue(), false)) {
          return false;
        }
        totalFieldsAuthorized++;
      }
    }
    if (merchantStatusType.getType() >= MerchantStatusType.BFB.getType()) {
      if (headers.containsValue(BulkParameters.AMPHI_BFB_STATUS) || headers.containsValue(
        BulkParameters.EXTERNAL_BFB_STATUS)) {
        totalFieldsAuthorized++;
      }
      if (headers.containsValue(BulkParameters.BFB_BASE_PRICE)) {
        totalFieldsAuthorized++;
      }
      if (headers.containsValue(BulkParameters.BFB_MANAGED)) {
        totalFieldsAuthorized++;
      }
    }
    if (validateBulkUpdateNewHeaders(headers))
      return false;

    if (validateHeadersOnPrivileges && !validateAllRequiredHeadersPresentForPrivileges(headerMap,
        availableHeaders, privilegedMap, includePoQuotaHeaderValidation)) {
      return false;
    }

    if (totalFieldsAuthorized == headers.size()) {
      return true;
    }
    return false;
  }

  public boolean authorizeUploadBulkUpdateEAN(BulkProcess bulkProcess, BulkUpdateErrorCounter counter, String storeId,
      Map<Integer, String> headers, String bulkProcessCode, String businessPartnerCode) throws IOException {
    if (MapUtils.isEmpty(headers)) {
      LOGGER.error(
          "No data found in file for bulk update EAN. storeId: {}, bulkProcessCode: {}, " + "businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode);
      updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }
    return EAN_UPLOAD_REQUIRED_HEADERS.stream().allMatch(headers::containsValue);
  }

  public boolean validateAllRequiredHeadersPresentForPrivileges(Map<String, String> headerMap,
      Set<String> availableHeaders, Map<String, Boolean> privilegedMap, boolean includePoQuotaHeaderValidation) {
    Map<String, List<String>> privilegesToHeaderMap = new HashMap<>();
    for (Map.Entry<String, String> entry : headerMap.entrySet()) {
      privilegesToHeaderMap.computeIfAbsent(entry.getValue(), k -> new ArrayList<>())
          .add(entry.getKey());
    }
    for (Map.Entry<String, Boolean> entry : privilegedMap.entrySet()) {
      if (Boolean.TRUE.equals(entry.getValue())) {
        if (BulkParameters.PRIVILEGE_EDIT_STOCK.equals(entry.getKey()) && !includePoQuotaHeaderValidation) {
          privilegesToHeaderMap.get(BulkParameters.PRIVILEGE_EDIT_STOCK).remove(BulkParameters.PO_QUOTA);
        }
        List<String> headersList =
            privilegesToHeaderMap.getOrDefault(entry.getKey(), Collections.emptyList());
        if (BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE.equals(entry.getKey())) {
          boolean isOnlyExternalUser = Boolean.TRUE.equals(privilegedMap.get("isOnlyExternalUser"));
          boolean hasCncPrivilege = Boolean.TRUE.equals(privilegedMap.get(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS));

          String statusHeader = isOnlyExternalUser
              ? BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P
              : BulkParameters.AMPHI_SKU_STATUS_CNC_1P;

          boolean hasDeliveryStatus = availableHeaders.contains(BulkParameters.DELIVERY_STATUS_HEADER);
          boolean hasStatusHeader = availableHeaders.contains(statusHeader);

          boolean isValid = hasCncPrivilege ? (hasStatusHeader && hasDeliveryStatus) : hasStatusHeader;

          if (!isValid) {
            log.error("Required headers for privilege {} are not present in the uploaded file. " +
                    "Required headers: {}, Available headers: {}",
                entry.getKey(), headersList, availableHeaders);
            return false;
          }
        }
        else {
          if (!availableHeaders.containsAll(headersList)) {
            log.error("Required headers for privilege {} are not present in the uploaded file. "
                    + "Required headers: {}, Available headers: {}", entry.getKey(), headersList,
                availableHeaders);
            return false;
          }
        }
      }
    }
    return true;
  }

  public static boolean validateBulkUpdateNewHeaders(Map<Integer, String> headers) {
    if (headers.containsValue(BulkParameters.PICKUP_POINT_HEADER) && !headers.containsValue(
        BulkParameters.PICKUP_POINT_NAME_COLUMN_ID)) {
      return true;
    }
    if (headers.containsValue(BulkParameters.STOCK_HEADER) && !headers.containsValue(
        BulkParameters.STOCK_REMINDER_COLUMN_ID)) {
      return true;
    }
    return false;
  }

  private boolean checkIfHeadersAreValidForBulkUpdate(Map<Integer, String> headers) {
    if (bulkUpdateHeaderValidation) {
      boolean allRequiredHeadersPresent = REQUIRED_HEADERS.stream().allMatch(headers::containsValue);
      boolean statusHeaderPresent;
      if (cncForWarehouseFeatureSwitch) {
        statusHeaderPresent = headers.containsValue(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P)
            || headers.containsValue(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);
      } else {
        statusHeaderPresent =
            headers.containsValue(BulkParameters.EXTERNAL_SKU_STATUS) || headers.containsValue(
                BulkParameters.AMPHI_SKU_STATUS);
      }
      return allRequiredHeadersPresent && statusHeaderPresent;
    }
    return true;
  }

  public boolean authorizeUploadCampaignProductBulkUpdate(Sheet workSheet, BulkProcess bulkProcess,
      BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter) throws IOException {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    final String businessPartnerCode = bulkUpdateQueue.getBusinessPartnerCode();
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(workSheet, 5);

    if (headers.size() == 0) {
      LOGGER.error(
          "No data found in file for bulk update. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
      updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }

    int totalFieldsAuthorized = 0;
    if (headers.containsValue(BulkParameters.BLIBLI_SKU)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.PICKUP_POINT_CODE)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.PICKUP_POINT_NAME)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.NAMA_PRODUK)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.SKU_NAME)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.HARGA_NORMAL)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.HARGA_JUAL)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.POTONGAN_HARGA)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.PERSENTASE_DISKON)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.STOK_TERSEDIA)) {
      totalFieldsAuthorized++;
    }
    if (pricingCampaignRecommendationEnabled && headers.containsValue(BulkParameters.HARGA_REKOMENDASI)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.REKOMENDASI)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.HARGA_AKHIR)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.KUOTA)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.KOMENTAR)) {
      totalFieldsAuthorized++;
    }

    return totalFieldsAuthorized == headers.size();
  }

  public List<BulkUpdateErrorDTO> validateExcelDatasBulkUpdateProduct(List<Map<String, String>> cleanDatas,
      List<String> pickupPointCodes, List<Map<String, String>> successDatas, List<Map<String, String>> failureDatas,
      BulkUpdateErrorCounter counter, int minimumPrice, Set<String> accessiblePickupPoints, boolean externalBulkUpdate,
      String businessPartnerCode) {
    if (CollectionUtils.isEmpty(cleanDatas)) {
      return new ArrayList<>();
    }
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    StringBuilder tempErrorMessage;
    for (Map<String, String> cleanData : cleanDatas) {
      tempErrorMessage = new StringBuilder();
      int stock = 0;
      boolean successFlag = true;
      successFlag = validateMandatoryColumn(counter, cleanData, successFlag, tempErrorMessage, BulkParameters.BLIBLI_SKU);
      if (externalBulkUpdate && StringUtils.isNotBlank(businessPartnerCode)) {
        successFlag = validateBlibliSkuAndProductSkuPartOfSeller(counter, businessPartnerCode, cleanData, successFlag, tempErrorMessage);
      }
      successFlag = successFlag && validateDatasForPrice(cleanData, tempErrorMessage, 0, 0, counter,
        minimumPrice) && validateDataForBfbFields(cleanData, tempErrorMessage, minimumPrice);
      if (cleanData.containsKey(BulkParameters.PO_QUOTA) && (!"-".equals(cleanData.get(BulkParameters.PO_QUOTA)
          .trim()))) {
        successFlag = validateDatasForPo_Quota(counter,
            tempErrorMessage,
            cleanData,
            stock,
            successFlag,
            cleanData.get(BulkParameters.PO_QUOTA));
      }
      if (cleanData.containsKey(BulkParameters.STOCK_HEADER) && (!"-".equals(cleanData.get(BulkParameters.STOCK_HEADER)
          .trim()))) {
        successFlag = validateDatasForStock(counter,
            tempErrorMessage,
            cleanData,
            stock,
            successFlag,
            cleanData.get(BulkParameters.STOCK_HEADER));
      }
      if (cleanData.containsKey(BulkParameters.STOCK_REMINDER_COLUMN_ID)) {
        successFlag = validateDatasForStock(counter, tempErrorMessage, cleanData, stock, successFlag,
            cleanData.get(BulkParameters.STOCK_REMINDER_COLUMN_ID));
      }
      if (cleanData.containsKey(BulkParameters.PICKUP_POINT_HEADER) && StringUtils.isEmpty(cleanData
          .get(BulkParameters.PICKUP_POINT_HEADER))) {
        successFlag = false;
        counter.incrementPickupPointCounter();
        if (counter.getPickupPointCounter() <= ERROR_COUNT) {
          tempErrorMessage.append(PICKUP_POINT_EMPTY).append(AND_SYMBOL);
        }
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
            cleanData.get(BulkParameters.BLIBLI_SKU),
            PICKUP_POINT_EMPTY);
      } else if (cleanData.containsKey(BulkParameters.PICKUP_POINT_HEADER)
          && !pickupPointCodes.contains(cleanData.get(BulkParameters.PICKUP_POINT_HEADER))) {
        successFlag = false;
        counter.incrementPickupPointCounter();
        if (counter.getPickupPointCounter() <= ERROR_COUNT) {
          tempErrorMessage.append(PICKUP_POINT_INVALID).append(AND_SYMBOL);
        }
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
                cleanData.get(BulkParameters.BLIBLI_SKU), PICKUP_POINT_INVALID);
      } else if (cleanData.containsKey(BulkParameters.PICKUP_POINT_HEADER)
          && CollectionUtils.isNotEmpty(accessiblePickupPoints)
          && !accessiblePickupPoints.contains(cleanData.get(BulkParameters.PICKUP_POINT_HEADER))) {
        successFlag = false;
        counter.incrementPickupPointCounter();
        if (counter.getPickupPointCounter() <= ERROR_COUNT) {
          tempErrorMessage.append(PICKUP_POINT_NOT_ACCESSIBLE).append(AND_SYMBOL);
        }
        LOGGER.error("Accessible pickup point validation failed for : BlibliSku {} - error msg - {}",
            cleanData.get(BulkParameters.BLIBLI_SKU), PICKUP_POINT_NOT_ACCESSIBLE);
      } else if (isSellerSkuInvalid(cleanData)) {
        successFlag = validateDatasForSellerSku(counter, tempErrorMessage, cleanData);
      }
      successFlag = successFlag && validateDatasForBooleanHeaders(cleanData, tempErrorMessage, counter);
      updateErrorDTOListForL3Update(tempErrorMessage, successFlag, cleanData, successDatas, failureDatas,
          bulkUpdateErrorDTOList, counter);
    }
    return bulkUpdateErrorDTOList;
  }

  public List<BulkUpdateErrorDTO> validateExcelDataBulkUpdateEANProduct(List<Map<String, String>> cleanDataList,
      List<Map<String, String>> successList, List<Map<String, String>> failureList, BulkUpdateErrorCounter counter,
      String businessPartnerCode) {
    if (CollectionUtils.isEmpty(cleanDataList)) {
      return Collections.emptyList();
    }
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    StringBuilder tempErrorMessage;
    for (Map<String, String> cleanData : cleanDataList) {
      tempErrorMessage = new StringBuilder();
      boolean successFlag = true;
      successFlag =
          validateMandatoryColumn(counter, cleanData, successFlag, tempErrorMessage, BulkParameters.BLIBLI_SKU);
      successFlag =
          validateMandatoryColumn(counter, cleanData, successFlag, tempErrorMessage, BulkParameters.BLIBLI_PRODUCT_SKU);
      if (StringUtils.isNotBlank(businessPartnerCode)) {
        successFlag = validateBlibliSkuAndProductSkuPartOfSeller(counter, businessPartnerCode, cleanData, successFlag,
            tempErrorMessage);
      }
      updateErrorDTOListForL4EANUpdate(tempErrorMessage, successFlag, cleanData, successList, failureList,
          bulkUpdateErrorDTOList, counter);
    }
    return bulkUpdateErrorDTOList;
  }

  public static boolean validateDatasForSellerSku(BulkUpdateErrorCounter counter, StringBuilder tempErrorMessage,
      Map<String, String> cleanData) {
    boolean successFlag;
    successFlag = false;
    counter.incrementSellerSkuCounter();
    if (counter.getSellerSkuCounter() <= ERROR_COUNT) {
      tempErrorMessage.append(SELLER_SKU_INVALID).append(AND_SYMBOL);
    }
    LOGGER.error("Validation error : BlibliSku {} - error msg - {}", cleanData.get(BulkParameters.BLIBLI_SKU),
        SELLER_SKU_INVALID);
    return successFlag;
  }

  public static boolean isSellerSkuInvalid(Map<String, String> cleanData) {
    return cleanData.containsKey(BulkParameters.SELLER_SKU) && StringUtils.isNotEmpty(
        cleanData.get(BulkParameters.SELLER_SKU))
        && cleanData.get(BulkParameters.SELLER_SKU).length() > SELLER_SKU_LIMIT;
  }

  public static boolean validateDatasForStock(BulkUpdateErrorCounter counter, StringBuilder tempErrorMessage,
      Map<String, String> cleanData, int stock, boolean successFlag, String stockHeader) {
    try {
      Double cleanDataStock = Double.parseDouble(stockHeader);
      stock = cleanDataStock.intValue();
    } catch (Exception e) {
      successFlag = false;
      counter.incrementStockCounter();
      if (counter.getStockCounter() <= ERROR_COUNT) {
        tempErrorMessage.append(STOCK_MUST_BE_NUMBER).append(AND_SYMBOL);
      }
      LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
          cleanData.get(BulkParameters.BLIBLI_SKU),
          STOCK_MUST_BE_NUMBER);
    }
    if (stock < 0) {
      successFlag = false;
      counter.incrementStockCounter();
      if (counter.getStockCounter() <= ERROR_COUNT) {
        tempErrorMessage.append(STOCK_MUST_BE_POSITIVE_VALUE).append(AND_SYMBOL);
      }
      LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
          cleanData.get(BulkParameters.BLIBLI_SKU),
          STOCK_MUST_BE_POSITIVE_VALUE);
    }
    return successFlag;
  }

  public static boolean validateDatasForPo_Quota(BulkUpdateErrorCounter counter, StringBuilder tempErrorMessage, Map<String, String> cleanData,
      int stock, boolean successFlag, String poQuotaHeader) {
    try {
      Double cleanDataStock = Double.valueOf(poQuotaHeader);
      stock = cleanDataStock.intValue();
    } catch (Exception e) {
      successFlag = false;
      counter.incrementPoQuotaCounter();
      if (counter.getPoQuotaCounter() <= ERROR_COUNT) {
        tempErrorMessage.append(PO_QUOTA_MUST_BE_NUMBER).append(AND_SYMBOL);
      }
      LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
          cleanData.get(BulkParameters.BLIBLI_SKU),
          PO_QUOTA_MUST_BE_NUMBER);
    }
    if (stock < 0) {
      successFlag = false;
      counter.incrementPoQuotaCounter();
      if (counter.getPoQuotaCounter() <= ERROR_COUNT) {
        tempErrorMessage.append(PO_QUOTA_MUST_BE_POSITIVE_VALUE).append(AND_SYMBOL);
      }
      LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
          cleanData.get(BulkParameters.BLIBLI_SKU),
          PO_QUOTA_MUST_BE_POSITIVE_VALUE);
    }
    return successFlag;
  }

  /**
   * check if brands is included in the campaign
   * @param brands
   * @param brand
   * @param errorMessage
   * @return true or false
   */
  private boolean checkBrandsIncludedInCampaign(List<String> brands, String brand,
      StringBuilder errorMessage) {
    if (brands.isEmpty() || brands.contains(brand)) {
      return true;
    }
    errorMessage.append(BRANDS_NOT_INCLUDED_IN_CAMPAIGN).append(AND_SYMBOL);
    return false;
  }

  /**
   * check if category code is included in the campaign
   * @param categories
   * @param category
   * @param errorMessage
   * @return true or false
   */
  private boolean checkCategoriesIncludedInCampaign(List<String> categories, String category,
      StringBuilder errorMessage) {
    if (categories.isEmpty() || categories.contains(category)) {
      return true;
    }
    errorMessage.append(CATEGORY_NOT_INCLUDED_IN_CAMPAIGN).append(AND_SYMBOL);
    return false;
  }

  /**
   * check if merchant code is included in a campaign
   * @param merchantCodeIncluded
   * @param merchantCode
   * @param errorMessage
   * @return true or false
   */
  private boolean checkMerchantCodeIncludedInCampaign(String merchantCodeIncluded, String merchantCode,
      StringBuilder errorMessage) {
    if (StringUtils.equals(merchantCode, merchantCodeIncluded)) {
      return true;
    }
    errorMessage.append(MERCHANT_CODE_NOT_INCLUDED_IN_CAMPAIGN).append(AND_SYMBOL);
    return false;
  }

  /**
   * check if a product is included in the campaign
   * @param bulkAddCampaignProductQueue
   * @param errorMessage
   * @param productLevel3SummaryResponse
   * @return true or false
   */
  private boolean isProductIncludedInCampaign(
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue, StringBuilder errorMessage,
      ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    return checkBrandsIncludedInCampaign(
        bulkAddCampaignProductQueue.getCampaignItemSummaryRequest().getBrands(),
        productLevel3SummaryResponse.getBrand(), errorMessage)
        && checkCategoriesIncludedInCampaign(
        bulkAddCampaignProductQueue.getCampaignItemSummaryRequest().getCategories(),
        productLevel3SummaryResponse.getCategoryCode(), errorMessage)
        && checkMerchantCodeIncludedInCampaign(
        bulkAddCampaignProductQueue.getCampaignItemSummaryRequest().getMerchantCode(),
        productLevel3SummaryResponse.getMerchantCode(), errorMessage);
  }

  /**
   * Check if input price cut greater than selling price
   *
   * @param errorMessage
   * @param tempCleanData - clean data from excel to get input price cut
   * @param productLevel3SummaryResponse - response from PBP to get selling price
   * @return true - if it is greater than used quota
   *         or
   *         false - otherwise
   */
  private boolean isPriceCutGreaterThanSellingPrice(StringBuilder errorMessage,
      Map<String, String> tempCleanData,
      ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    Double priceCut = Double.valueOf(tempCleanData.get(BulkParameters.POTONGAN_HARGA));
    Double salePrice = productLevel3SummaryResponse.getPrices().get(0).getSalePrice();
    if (priceCut > salePrice) {
      errorMessage.append(PRICE_CUT_CANT_BE_GREATER_THAN_SELLING_PRICE).append(AND_SYMBOL);
      return false;
    }
    priceCut = salePrice - Double.valueOf(tempCleanData.get(BulkParameters.HARGA_AKHIR));
    if (priceCut <= 0) {
      errorMessage.append(PRICE_CUT_CANNOT_BE_ZERO_AND_MUST_BE_POSITIVE_VALUE).append(AND_SYMBOL);
      return false;
    }
    return true;
  }

  /**
   * Check if input quota greater than used quota
   *
   * @param errorMessage
   * @param tempCleanData - clean data from excel to get input quota
   * @param campaignProductDetailResponse - response from x-campaign to get used quota
   * @return true - if it is greater than used quota
   *         or
   *         false - otherwise
   */
  private boolean isInputQuotaGreaterThanUsedQuota(StringBuilder errorMessage,
      Map<String, String> tempCleanData,
      CampaignProductDetailResponse campaignProductDetailResponse) {
    int inputQuota = Double.valueOf(tempCleanData.get(BulkParameters.KUOTA)).intValue();
    int usedQuota = campaignProductDetailResponse.getUsedQuota();
    if (inputQuota < usedQuota) {
      errorMessage.append(INPUT_QUOTA_CANT_BE_LESS_THAN_USED_QUOTA).append(AND_SYMBOL);
      return false;
    }
    return true;
  }

  /**
   * Validate BLIBLI SKU must be filled
   *
   * @param cleanData
   * @param tempErrorMessage
   */
  private static boolean checkBlibliSKU(Map<String, String> cleanData, StringBuilder tempErrorMessage) {
    if (cleanData.containsKey(BulkParameters.BLIBLI_SKU) && StringUtils.isBlank(cleanData.get(
        BulkParameters.BLIBLI_SKU))) {
      tempErrorMessage.append(BLIBLI_SKU_BLANK).append(AND_SYMBOL);
      LOGGER.error("Validation Error - Blibli SKU: {} - errorMessage: {}",
          cleanData.get(BulkParameters.BLIBLI_SKU), BLIBLI_SKU_BLANK);
      return false;
    }
    return true;
  }

  /**
   *
   * @param cleanData
   * @param header
   * @param headerValidationError
   * @param tempErrorMessage
   * @return
   */
  private static boolean checkHeaderAndValue(Map<String, String> cleanData, String header, String headerValidationError,
      StringBuilder tempErrorMessage) {
    if (cleanData.containsKey(header)) {
      if (StringUtils.isBlank(cleanData.get(header))) {
        tempErrorMessage.append(headerValidationError).append(AND_SYMBOL);
        LOGGER.error("Validation Error - header: {} - errorMessage: {}", cleanData.get(header), headerValidationError);
        return false;
      }
    } else {
      return false;
    }
    return true;
  }

  /**
   * validate price cut must be number and positive
   * @param cleanData
   * @param successFlag
   * @param tempErrorMessage
   */
  private boolean checkPriceCut(Map<String, String> cleanData, boolean successFlag,
      StringBuilder tempErrorMessage) {
    int priceCut = 0;
    boolean result = successFlag;
    if (cleanData.containsKey(BulkParameters.POTONGAN_HARGA)) {
      try {
        Double cleanDataPriceCut = Double.parseDouble(cleanData.get(BulkParameters.POTONGAN_HARGA));
        priceCut = cleanDataPriceCut.intValue();
      } catch (Exception e) {
        result = false;
        tempErrorMessage.append(PRICE_CUT_MUST_BE_NUMBER).append(AND_SYMBOL);
        LOGGER.error("Validation error : Potongan Harga = {} with errorMessage = {}",
            cleanData.get(BulkParameters.POTONGAN_HARGA), PRICE_CUT_MUST_BE_NUMBER);
      }

      if (priceCut <= 0) {
        result = false;
        tempErrorMessage.append(PRICE_CUT_CANNOT_BE_ZERO_AND_MUST_BE_POSITIVE_VALUE)
            .append(AND_SYMBOL);
        LOGGER.error("Validation error : Potongan Harga {} - error msg - {}",
            cleanData.get(BulkParameters.POTONGAN_HARGA),
            PRICE_CUT_CANNOT_BE_ZERO_AND_MUST_BE_POSITIVE_VALUE);
      }
    }
    return result;
  }

  private boolean checkQuota(Map<String, String> cleanData, boolean successFlag, StringBuilder tempErrorMessage,
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue) {
    int kuota = 0;
    try {
      kuota = Double.valueOf(cleanData.get(BulkParameters.KUOTA)).intValue();
    } catch (Exception e) {
      LOGGER.error("Validation error : Min kuota = {} with errorMessage = {}",
          cleanData.get(BulkParameters.KUOTA), MIN_KUOTA_MUST_BE_NUMBER);
      return false;
    }
    if (kuota < bulkAddCampaignProductQueue.getMinQuota()) {
      successFlag = false;
      tempErrorMessage.append(PRODUCT_KUOTA_LESS_THAN_MIN_QUOTA)
          .append(bulkAddCampaignProductQueue.getMinQuota())
          .append(AND_SYMBOL);
      LOGGER.error("Validation error : Min kuota {} - error msg - {}",
          cleanData.get(BulkParameters.KUOTA), PRODUCT_KUOTA_LESS_THAN_MIN_QUOTA);
    }

    return successFlag;
  }

  /**
   * validate field value must be number and greater than zero
   *
   * @param cleanData must not be null
   * @param successFlag true if success, otherwise false
   * @param tempErrorMessage error message that will be shown in notification
   * @param parameter header name
   */
  private static boolean checkFieldValueMustBeNumberAndPositive(Map<String, String> cleanData,
      boolean successFlag, StringBuilder tempErrorMessage, String parameter) {
    boolean result = successFlag;
    if (cleanData.containsKey(parameter)) {
      Pair<Integer, Boolean> numberAndResultPair = checkParameterValueMustBeNumber(
          cleanData.get(parameter), result, tempErrorMessage, parameter);
      result = numberAndResultPair.getRight();
      if (numberAndResultPair.getLeft() < 0) {
        result = false;
        tempErrorMessage.append(parameter).append(SPACE).append(MUST_BE_POSITIVE_VALUE)
            .append(AND_SYMBOL);
        LOGGER.error("Validation Error - " + parameter + " {} - error message: {}",
            cleanData.get(parameter), MUST_BE_POSITIVE_VALUE);
      }
    }
    return result;
  }


  /**
   * validate pickup point value not must be empty
   *
   * @param rowCleanData must not be null
   * @param successFlag true if success, otherwise false
   * @param tempErrorMessage error message that will be shown in notification
   * @param pickupPointCode header name
   * @param pickupPointEmptyMessage text error message
   */

  private static boolean checkPickupPointNotEmpty(Map<String, String> rowCleanData,
    boolean successFlag, String pickupPointCode, String pickupPointEmptyMessage,
    StringBuilder tempErrorMessage) {
    boolean result = successFlag;
    if (rowCleanData.containsKey(pickupPointCode)) {
      if (StringUtils.isBlank(rowCleanData.get(pickupPointCode))) {
        tempErrorMessage.append(pickupPointEmptyMessage).append(AND_SYMBOL);
        LOGGER.error("Validation Error - header: {} - errorMessage: {} ",
          rowCleanData.get(pickupPointCode), tempErrorMessage);
        result = false;
      } else {
        result = true;
      }
    }
    return result;
  }

  /**
   * validate field value must be number and greater than zero
   *
   * @param cleanData must not be null
   * @param successFlag true if success, otherwise false
   * @param tempErrorMessage error message that will be shown in notification
   * @param parameter header name
   */
  private static boolean checkFieldValueMustBeNumberAndGreaterThanZero(
      Map<String, String> cleanData, boolean successFlag, StringBuilder tempErrorMessage,
      String parameter) {
    boolean result = successFlag;
    if (cleanData.containsKey(parameter)) {
      Pair<Integer, Boolean> numberAndResultPair = checkParameterValueMustBeNumber(
          cleanData.get(parameter), result, tempErrorMessage, parameter);
      result = numberAndResultPair.getRight();
      if (numberAndResultPair.getLeft() <= 0) {
        result = false;
        tempErrorMessage.append(parameter).append(SPACE)
            .append(CANNOT_BE_ZERO_AND_MUST_BE_POSITIVE_VALUE).append(AND_SYMBOL);
        LOGGER.error("Validation Error - {} {} - error message: {}", parameter,
            cleanData.get(parameter), CANNOT_BE_ZERO_AND_MUST_BE_POSITIVE_VALUE);
      }
    }
    return result;
  }

  private static Pair<Integer, Boolean> checkParameterValueMustBeNumber(String parameterValue,
      boolean result, StringBuilder tempErrorMessage, String parameter) {
    int number = 0;
    try {
      double cleanDataDoubleValue = Double.parseDouble(parameterValue);
      number = (int) cleanDataDoubleValue;
    } catch (Exception e) {
      result = false;
      tempErrorMessage.append(parameter).append(SPACE).append(MUST_BE_NUMBER).append(AND_SYMBOL);
      LOGGER.error("Validation Error - {} {} - error message: {}", parameter, parameterValue,
          MUST_BE_NUMBER);
    }
    return Pair.of(number, result);
  }

  /**
   * Get Campaign Product Details and Validate Input Quota can't be less than Used Quota
   * <p>
   * Find Data From PBP and add product that is included in campaign to success list, and the rest to
   * failure list
   *
   * @param itemSkuList
   * @param rowNumber
   * @param cleanDatas
   * @param tempErrorMessage
   * @param bulkAddCampaignProductQueue
   * @param tempCleanDataList
   * @param successDatas
   * @param failureDataList
   * @param offlineItemIdBulkUpdateErrorMap
   * @param bulkUpdateErrorCounter
   * @param cleanData
   * @param itemSkuPPCodeList
   */
  private void validateQuotaAndPricesForCampaign(List<String> itemSkuList, int rowNumber,
    List<Map<String, String>> cleanDatas, StringBuilder tempErrorMessage,
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue,
    List<Map<String, String>> tempCleanDataList, List<Map<String, String>> successDatas,
    List<Map<String, String>> failureDataList,
    Map<String, BulkUpdateErrorDTO> offlineItemIdBulkUpdateErrorMap,
    BulkUpdateErrorCounter bulkUpdateErrorCounter, Map<String, String> cleanData,
    List<String> itemSkuPPCodeList) {
    if (itemSkuList.size() == GET_BATCH_SIZE || (!itemSkuList.isEmpty()
      && rowNumber == cleanDatas.size())) {
      try {
        //TODO add v2 campaign update call
        boolean successFlag = false;
        if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
          Map<String, CampaignProductDetailResponse> campaignProductDetailResponseMap =
              this.getCampaignProductDetailsV2(cleanDatas, bulkAddCampaignProductQueue);
          Map<String, ItemSummaryListResponse> itemSummaryMap = this.getItemSummaryData(cleanDatas);
          pureCNCCheckWithOnlyPricingEnabled(itemSummaryMap);
          BulkCreationCommonUtil.toAddDetailsForCampaignV2(itemSummaryMap, cleanDatas);
          int itemSkuIndex = 0;
          for (String itemSkuPPCode : itemSkuPPCodeList) {

            tempErrorMessage = new StringBuilder().append(BulkParameters.ITEM_SKU_PP_CODE_EN).append(SPACE).append(IS)
                .append(itemSkuPPCode).append(END_SYMBOL).append(FAILURE_REASON).append(IS);

            BulkUpdateServiceValidation bulkUpdateServiceValidation =
                BulkUpdateServiceValidation.builder().tempCleanDataList(tempCleanDataList)
                    .bulkAddCampaignProductQueue(bulkAddCampaignProductQueue)
                    .campaignProductDetailResponseMap(campaignProductDetailResponseMap).itemSummaryMap(itemSummaryMap)
                    .tempErrorMessage(tempErrorMessage).build();

            if (itemSummaryMap.containsKey(itemSkuPPCode)) {
              if (campaignProductDetailResponseMap.containsKey(itemSkuPPCode)) {
                successFlag = isQuotaAndPriceValidatedV2(bulkUpdateServiceValidation, itemSkuIndex, itemSkuPPCode);
              } else {
                successFlag =
                    isPriceValidatedWhenCampaignEqualsNullV2(bulkUpdateServiceValidation, itemSkuIndex, itemSkuPPCode);
              }

            } else {
              successFlag = false;
              tempErrorMessage.append(FAILED_DATA_NOT_FOUND).append(AND_SYMBOL);
            }
            updateErrorDTOList(tempErrorMessage, successFlag, tempCleanDataList.get(itemSkuIndex), successDatas,
                failureDataList, offlineItemIdBulkUpdateErrorMap, bulkUpdateErrorCounter);
            itemSkuIndex++;
          }
        }
      } catch (Exception e) {
        tempErrorMessage.append(FAILED_TO_GET_DATA).append(AND_SYMBOL);
        LOGGER.error("Failed to get response from client with Blibli SKU = {} and error message = {}",
            cleanData.get(BulkParameters.BLIBLI_SKU), e.getMessage(), e);
        updateErrorDTOList(tempErrorMessage, tempCleanDataList, failureDataList,
          offlineItemIdBulkUpdateErrorMap, cleanDatas);
      } finally {
        itemSkuList.clear();
        tempCleanDataList.clear();
      }
    }
  }

  private void pureCNCCheckWithOnlyPricingEnabled(Map<String, ItemSummaryListResponse> itemSummaryMap) {
    if (pricingMultiPickupPointEnabled && Boolean.FALSE.equals(multiPickupPointEnabled)) {
      List<String> pureCncL5 =
        itemSummaryMap.values().stream().filter(BulkUpdateServiceUtil::isPureCnc)
          .map((i) -> i.getItemSku() + Constant.HYPHEN + i.getPickupPointCode())
          .collect(Collectors.toList());
      for (String data : pureCncL5) {
        itemSummaryMap.remove(data);
      }
    }
  }

  private static Boolean isPureCnc(ItemSummaryListResponse itemSummaryListResponse) {
    return itemSummaryListResponse.isCncActive() && itemSummaryListResponse.getItemViewConfigs()
      .iterator().hasNext() && (
      !itemSummaryListResponse.getItemViewConfigs().iterator().next().isBuyable()
        && !itemSummaryListResponse.getItemViewConfigs().iterator().next().isDiscoverable());
  }

  private static void successDataForMppEnabled(List<Map<String, String>> cleanDatas) {
    for (Map<String, String> cleanDataMap : cleanDatas) {
      cleanDataMap.put(BulkParameters.ITEM_SKU_PP_CODE,
        cleanDataMap.get(BulkParameters.BLIBLI_SKU) + Constant.DASH + cleanDataMap
          .get(BulkParameters.PICKUP_POINT_CODE));
    }
  }

  public List<BulkUpdateErrorDTO> validateExcelDatasBulkUpdateCampaignProduct(
      List<Map<String, String>> cleanDatas, List<Map<String, String>> successDatas,
      List<Map<String, String>> failureDatas, BulkUpdateErrorCounter bulkUpdateErrorCounter,
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue) {
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    Map<String , BulkUpdateErrorDTO> offlineItemIdBulkUpdateErrorMap = new HashMap<>();

    if (CollectionUtils.isEmpty(cleanDatas)) {
      return bulkUpdateErrorDTOList;
    }
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      successDataForMppEnabled(cleanDatas);
      validateDuplicateItemSkuAndPPCode(cleanDatas, bulkUpdateErrorDTOList, bulkUpdateErrorCounter);
    } else {
      validateDuplicateSku(cleanDatas, bulkUpdateErrorDTOList, bulkUpdateErrorCounter);
    }
    if (CollectionUtils.isNotEmpty(cleanDatas)) {
      setChildCategories(bulkAddCampaignProductQueue);
    }

    StringBuilder tempErrorMessage;
    List<String> itemSkuList = new ArrayList<>();
    List<String> pickupPointCodeList = new ArrayList<>();
    List<String> itemSkuPPCodeList = new ArrayList<>();
    List<Map<String, String>> tempCleanDatas = new ArrayList<>();
    int rowNumber = 1;

    for (Map<String, String> cleanData : cleanDatas) {
      tempErrorMessage =
        new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(BulkUpdateServiceUtil.IS)
          .append(cleanData.get(BulkParameters.BLIBLI_SKU)).append(BulkUpdateServiceUtil.SPACE)
          .append(BulkParameters.PICKUP_POINT_CODE).append(BulkUpdateServiceUtil.IS)
          .append(cleanData.getOrDefault(BulkParameters.PICKUP_POINT_CODE, StringUtils.EMPTY))
          .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILURE_REASON)
          .append(BulkUpdateServiceUtil.IS);

      boolean successFlag = checkBlibliSKU(cleanData, tempErrorMessage);
      if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
        successFlag =
            checkHeaderAndValue(cleanData, BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_EMPTY, tempErrorMessage);
      }
      successFlag =
          checkPriceCut(cleanData, successFlag, tempErrorMessage);
      successFlag = checkFieldValueMustBeNumberAndGreaterThanZero(cleanData, successFlag,
          tempErrorMessage, BulkParameters.KUOTA);
      successFlag = checkQuota(cleanData, successFlag, tempErrorMessage, bulkAddCampaignProductQueue);

      if (successFlag) {
        itemSkuList.add(cleanData.get(BulkParameters.BLIBLI_SKU));
        pickupPointCodeList.add(cleanData.get(BulkParameters.PICKUP_POINT_CODE));
        itemSkuPPCodeList.add(cleanData.get(BulkParameters.ITEM_SKU_PP_CODE));
        tempCleanDatas.add(cleanData);
      } else {
        updateErrorDTOList(tempErrorMessage, false, cleanData, successDatas, failureDatas,
          offlineItemIdBulkUpdateErrorMap, bulkUpdateErrorCounter);
      }

      this.validateQuotaAndPricesForCampaign(itemSkuList, rowNumber, cleanDatas, tempErrorMessage,
        bulkAddCampaignProductQueue, tempCleanDatas, successDatas, failureDatas,
        offlineItemIdBulkUpdateErrorMap, bulkUpdateErrorCounter, cleanData, itemSkuPPCodeList);

      rowNumber++;
    }
    bulkUpdateErrorDTOList.addAll(offlineItemIdBulkUpdateErrorMap.values());
    return bulkUpdateErrorDTOList;
  }

  private void setChildCategories(BulkAddCampaignProductQueue bulkAddCampaignProductQueue) {
    List<String> categoryCodes = Optional.ofNullable(bulkAddCampaignProductQueue)
        .map(BulkAddCampaignProductQueue::getCampaignItemSummaryRequest)
        .map(CampaignItemSummaryRequest::getCategories).map(categories -> new ArrayList(categories))
        .orElse(new ArrayList());
    List<String> childCategories = new ArrayList<>();
    for (String categoryCode : categoryCodes) {
      try {
        Optional.ofNullable(pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(
          bulkAddCampaignProductQueue.getRequestId(),
          new CategoryCodeRequest(Collections.singletonList(categoryCode)),
          bulkAddCampaignProductQueue.isFilterOutInactiveCn())).ifPresent(childCategories::addAll);
      } catch (Exception ex) {
        LOGGER.error("Error while getting Child categories for category code : {}", categoryCode, ex);
      }
    }
    categoryCodes.addAll(childCategories);
    bulkAddCampaignProductQueue.getCampaignItemSummaryRequest().setCategories(categoryCodes);
  }

  private static void updateErrorDTOList(StringBuilder tempErrorMessage, boolean successFlag,
    Map<String, String> cleanData, List<Map<String, String>> successDataList,
    List<Map<String, String>> failureDataList,
    Map<String, BulkUpdateErrorDTO> offlineItemIdBulkUpdateErrorMap,
    BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    if (successFlag) {
      successDataList.add(cleanData);
    } else {
      failureDataList.add(cleanData);
      bulkUpdateErrorCounter.incrementInputErrorCounter();
      String errorMessage = removeLastAmpSymbol(tempErrorMessage.toString());
      String offlineItemId = cleanData.get(BulkParameters.BLIBLI_SKU) + Constant.HYPHEN + cleanData.get(
        BulkParameters.PICKUP_POINT_CODE);
      BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO(cleanData.get(BulkParameters.PRODUCT_NAME),
          cleanData.get(BulkParameters.BLIBLI_SKU), cleanData.get(BulkParameters.PICKUP_POINT_CODE),
          errorMessage);
      offlineItemIdBulkUpdateErrorMap.put(offlineItemId, bulkUpdateErrorDTO);
    }
  }

  private static void updateErrorDTOListForL3Update(StringBuilder tempErrorMessage,
    boolean successFlag, Map<String, String> cleanData, List<Map<String, String>> successDataList,
    List<Map<String, String>> failureDataList, List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList,
    BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    if (successFlag) {
      successDataList.add(cleanData);
    } else {
      failureDataList.add(cleanData);
      bulkUpdateErrorCounter.incrementInputErrorCounter();
      String errorMessage = removeLastAmpSymbol(tempErrorMessage.toString());
      bulkUpdateErrorDTOList.add(new BulkUpdateErrorDTO(cleanData.get(BulkParameters.PRODUCT_NAME),
        cleanData.get(BulkParameters.BLIBLI_SKU), cleanData.get(BulkParameters.PICKUP_POINT_HEADER),
        errorMessage));
    }
  }

  private static void updateErrorDTOListForL4EANUpdate(StringBuilder tempErrorMessage, boolean successFlag,
      Map<String, String> cleanData, List<Map<String, String>> successDataList,
      List<Map<String, String>> failureDataList, List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList,
      BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    if (successFlag) {
      successDataList.add(cleanData);
    } else {
      failureDataList.add(cleanData);
      bulkUpdateErrorCounter.incrementInputErrorCounter();
      String errorMessage = removeLastAmpSymbol(tempErrorMessage.toString());
      bulkUpdateErrorDTOList.add(
          new BulkUpdateErrorDTO(cleanData.get(BulkParameters.NAMA_PRODUK), cleanData.get(BulkParameters.BLIBLI_SKU),
              errorMessage));
    }
  }

  private static void validateAndAddSuccessAndFailedDataForCncBulkUpload(
      StringBuilder tempErrorMessage, boolean successFlag, Map<String, String> cleanData,
      List<Map<String, String>> successDataList, List<Map<String, String>> failureDataList,
      List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList,
      BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    if (successFlag) {
      successDataList.add(cleanData);
    } else {
      failureDataList.add(cleanData);
      bulkUpdateErrorCounter.incrementInputErrorCounter();
      String errorMessage = removeLastAmpSymbol(tempErrorMessage.toString());
      bulkCncUpsertErrorDTOList.add(new BulkCncUpsertErrorDTO(
          Optional.ofNullable(cleanData.get(BulkParameters.BLIBLI_SKU))
              .orElseGet(() -> cleanData.get(BulkParameters.BLIBLI_SKU_WITH_EXAMPLE)),
          cleanData.get(BulkParameters.PICKUP_POINT_CODE), errorMessage));
    }
  }

  private void updateErrorDTOList(StringBuilder tempErrorMessage,
    List<Map<String, String>> tempCleanDatas, List<Map<String, String>> failureDatas,
    Map<String, BulkUpdateErrorDTO> offlineItemIdBulkUpdateErrorMap,
    List<Map<String, String>> cleanDatas) {
    for (Map<String, String> cleanData : cleanDatas) {
      String errorMessage = removeLastAmpSymbol(tempErrorMessage.toString());
      String offlineItemId =
        cleanData.getOrDefault(BulkParameters.BLIBLI_SKU, StringUtils.EMPTY) + Constant.HYPHEN
          + cleanData.getOrDefault(BulkParameters.PICKUP_POINT_CODE, StringUtils.EMPTY);
      BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO(StringUtils.EMPTY,
        cleanData.getOrDefault(BulkParameters.BLIBLI_SKU, StringUtils.EMPTY),
        cleanData.getOrDefault(BulkParameters.PICKUP_POINT_CODE, StringUtils.EMPTY), errorMessage);
      offlineItemIdBulkUpdateErrorMap.put(offlineItemId, bulkUpdateErrorDTO);
    }
    failureDatas.addAll(tempCleanDatas);
  }

  private static String removeLastAmpSymbol(String tempErrorMessage) {
    if (tempErrorMessage.length() > 0) {
      tempErrorMessage = tempErrorMessage.substring(0, tempErrorMessage.length() - 2);
    }
    return tempErrorMessage;
  }

  private static void updateErrorDTOList(List<Map<String, String>> cleanDatas, Set<String> failureDatas,
      List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList, BulkUpdateErrorCounter counter) {
    List<Map<String, String>> datasToBeRemoved = new ArrayList<>();
    counter.incrementInputErrorCounter();
    failureDatas.forEach(failureData -> {
      StringBuilder reasonMessage = new StringBuilder().append(BulkParameters.BLIBLI_SKU)
          .append(BulkUpdateServiceUtil.IS).append(failureData)
          .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILURE_REASON)
          .append(BulkUpdateServiceUtil.IS).append(BulkUpdateServiceUtil.DUPLICATE_BLIBLI_SKU);
      bulkUpdateErrorDTOList
          .add(new BulkUpdateErrorDTO(null, failureData, reasonMessage.toString()));
      datasToBeRemoved.addAll(cleanDatas.stream()
          .filter(cleanData -> cleanData.get(BulkParameters.BLIBLI_SKU).equals(failureData))
          .collect(Collectors.toList()));
    });
    cleanDatas.removeAll(datasToBeRemoved);
  }

  private static void updateErrorDTOListForMppProduct(List<Map<String, String>> cleanDatas,
   Set<String> failureDatas,
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList, BulkUpdateErrorCounter counter) {
    List<Map<String, String>> datasToBeRemoved = new ArrayList<>();
    counter.incrementInputErrorCounter();
    failureDatas.forEach(failureData -> {
      StringBuilder reasonMessage = new StringBuilder().append(BulkParameters.BLIBLI_SKU)
        .append(BulkUpdateServiceUtil.IS).append(failureData)
        .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILURE_REASON)
        .append(BulkUpdateServiceUtil.IS).append(BulkUpdateServiceUtil.DUPLICATE_OFFLINE_ITEM);
      bulkUpdateErrorDTOList
        .add(new BulkUpdateErrorDTO(null, failureData, reasonMessage.toString()));
      datasToBeRemoved.addAll(cleanDatas.stream()
        .filter(cleanData -> cleanData.get(BulkParameters.ITEM_SKU_PP_CODE).equals(failureData))
        .collect(Collectors.toList()));
    });
    cleanDatas.removeAll(datasToBeRemoved);
  }


  public static InputStream getFileInputStream(String bulkProcessCode, String fileName,
      String pathName) throws FileNotFoundException {
    return new FileInputStream(new File(pathName + bulkProcessCode + File.separator
        + bulkProcessCode + ProcessorUtils.getFileFormat(fileName)));
  }

  public static void updateBulkStatusAborted(BulkProcess bulkProcess, String storeId,
      String bulkProcessCode, String errorMessage, BulkUpdateErrorCounter counter) {
    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    setBulkProcessNotesAndErrorCount(bulkProcess, storeId, bulkProcessCode, errorMessage, counter);
  }

  private static void setBulkProcessNotesAndErrorCount(BulkProcess bulkProcess, String storeId,
      String bulkProcessCode, String errorMessage, BulkUpdateErrorCounter counter) {
    BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
    bulkProcessNotes.setStoreId(storeId);
    bulkProcessNotes.setBulkProcessCode(bulkProcessCode);
    bulkProcessNotes.setBulkProcess(bulkProcess);
    bulkProcessNotes.setNotes(errorMessage);
    if (bulkProcess.getBulkProcessNotes() != null) {
      bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
    } else {
      bulkProcess.setBulkProcessNotes(Collections.singletonList(bulkProcessNotes));
    }
    bulkProcess.setInputErrorCount(counter.getInputErrorCounter());
    bulkProcess.setSystemErrorCount(counter.getSystemErrorCounter());
  }

  public void updateBulkProcessNotes(List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, String storeId,
      String bulkProcessCode, BulkProcess bulkProcess, boolean isWholeSaleConfig, WholeSaleCount wholeSaleCount)
      throws JsonProcessingException {
    for (BulkUpdateErrorDTO bulkUpdateErrorDTO : listBulkUpdateErrorDTO) {
      BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
      bulkProcessNotes.setStoreId(storeId);
      bulkProcessNotes.setBulkProcessCode(bulkProcessCode);
      bulkProcessNotes.setBulkProcess(bulkProcess);
      if (StringUtils.isEmpty(bulkUpdateErrorDTO.getPickupPointCode())) {
        bulkProcessNotes.setNotes(new StringBuilder(BulkParameters.PRODUCT_NAME).append(IS)
          .append(bulkUpdateErrorDTO.getProductName()).append(AND_SYMBOL)
          .append(BulkParameters.BLIBLI_SKU).append(IS).append(bulkUpdateErrorDTO.getProductSku())
          .append(AND_SYMBOL).append(FAILURE_REASON).append(IS)
          .append(bulkUpdateErrorDTO.getReason()).toString());
      } else {
        bulkProcessNotes.setNotes(new StringBuilder(BulkParameters.PRODUCT_NAME).append(IS)
          .append(bulkUpdateErrorDTO.getProductName()).append(AND_SYMBOL)
          .append(BulkParameters.BLIBLI_SKU).append(IS).append(bulkUpdateErrorDTO.getProductSku())
          .append(AND_SYMBOL).append(BulkParameters.PICKUP_POINT_CODE).append(IS)
          .append(bulkUpdateErrorDTO.getPickupPointCode()).append(AND_SYMBOL).append(FAILURE_REASON)
          .append(IS).append(bulkUpdateErrorDTO.getReason()).toString());
      }
      if (bulkProcess.getBulkProcessNotes() != null) {
        bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      } else {
        bulkProcess.setBulkProcessNotes(Arrays.asList(bulkProcessNotes));
      }
    }
    if (isWholeSaleConfig) {
      BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
      bulkProcessNotes.setStoreId(storeId);
      bulkProcessNotes.setBulkProcessCode(bulkProcessCode);
      bulkProcessNotes.setBulkProcess(bulkProcess);
      bulkProcessNotes.setWholeSaleConfig(isWholeSaleConfig);
      bulkProcessNotes.setNotes(new ObjectMapper().writeValueAsString(wholeSaleCount));
      if (bulkProcess.getBulkProcessNotes() != null) {
        bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      } else {
        bulkProcess.setBulkProcessNotes(Arrays.asList(bulkProcessNotes));
      }
    }
  }

  public void updateBulkProcessNotesForPromoRegistered(String storeId, BulkUpdateSuccessDTO bulkUpdateSuccessDTO,
      BulkProcess bulkProcess) {
    BulkProcessNotes bulkProcessNote = new BulkProcessNotes();
    bulkProcessNote.setStoreId(storeId);
    bulkProcessNote.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessNote.setBulkProcess(bulkProcess);
    bulkProcessNote.setPromoNote(true);
    bulkProcessNote.setNotes(
        new StringBuilder(bulkUpdateSuccessDTO.getProductSku()).append(AND_SYMBOL)
            .append(bulkUpdateSuccessDTO.getProductName()).toString());
    if (bulkProcess.getBulkProcessNotes() != null) {
      bulkProcess.getBulkProcessNotes().add(bulkProcessNote);
    } else {
      bulkProcess.setBulkProcessNotes(new ArrayList<>());
      bulkProcess.getBulkProcessNotes().add(bulkProcessNote);
    }
  }

  public static void updateBulkProcessNotesForCampaignOrCncProduct(
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, String storeId, String bulkProcessCode,
      BulkProcess bulkProcess) {
    // update bulk process note for campaign or cnc product
  }

  public static void updateBulkErrors(List<Map<String, String>> failedProductList,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, BulkUpdateErrorCounter counter,
      List<Map<String, String>> validationPassedData,
    Set<String> listOfCampaignResponseFailedItemSkus,boolean isCampaign) {
    if (CollectionUtils.isNotEmpty(failedProductList)) {
      failedProductList.forEach((map) -> map.put(BulkParameters.ITEM_SKU_PP_CODE,
        map.getOrDefault(BulkParameters.BLIBLI_SKU, StringUtils.EMPTY) + Constant.DASH + map.getOrDefault(
          BulkParameters.PICKUP_POINT_CODE, StringUtils.EMPTY)));
      Optional.ofNullable(validationPassedData).orElse(new ArrayList<>()).forEach((map) -> map.put(BulkParameters.ITEM_SKU_PP_CODE,
        map.getOrDefault(BulkParameters.BLIBLI_SKU, StringUtils.EMPTY) + Constant.DASH + map.getOrDefault(
          BulkParameters.PICKUP_POINT_CODE, StringUtils.EMPTY)));
      for (Map<String, String> failedProductDetail : failedProductList) {
        if(!listOfCampaignResponseFailedItemSkus.contains(failedProductDetail.get(BulkParameters.BLIBLI_SKU))){
        counter.incrementSystemErrorCounter();
        String errorMessage = constructErrorMessageAndValidatePassedDataForBulkUpload(
            validationPassedData, failedProductDetail, isCampaign);
        if (counter.getSystemErrorCounter() <= ERROR_COUNT) {
          listBulkUpdateErrorDTO
              .add(new BulkUpdateErrorDTO(failedProductDetail.get(BulkParameters.PRODUCT_NAME),
                  failedProductDetail.get(BulkParameters.BLIBLI_SKU),
                failedProductDetail.get(BulkParameters.PICKUP_POINT_CODE), errorMessage));
          }
        }
      }
    }
  }

  public static void updateBulkErrorsL5(List<Map<String, String>> failedProductList,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, BulkUpdateErrorCounter counter,
      List<Map<String, String>> validationPassedData) {
    if (CollectionUtils.isNotEmpty(failedProductList)) {
      for (Map<String, String> failedProductDetail : failedProductList) {
        counter.incrementSystemErrorCounter();
        String errorMessage =
            constructErrorMessageAndValidatePassedDataForBulkUpload(validationPassedData, failedProductDetail, false);
        if (counter.getSystemErrorCounter() <= ERROR_COUNT) {
          listBulkUpdateErrorDTO.add(new BulkUpdateErrorDTO(failedProductDetail.get(BulkParameters.PRODUCT_NAME),
              failedProductDetail.get(BulkParameters.BLIBLI_SKU),
              failedProductDetail.get(BulkParameters.PICKUP_POINT_HEADER), errorMessage));
        }
      }
    }
  }

  public static void addErrorCounterAndConstructErrorMessageForCncBulkUpload(
      List<Map<String, String>> failedProductList,
      List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList,
      BulkUpdateErrorCounter bulkUpdateErrorCounter,
      List<Map<String, String>> validationPassedData) {
    for (Map<String, String> failedProductDetail : failedProductList) {
      bulkUpdateErrorCounter.incrementSystemErrorCounter();
      String errorMessage = constructErrorMessageAndValidatePassedDataForBulkUpload(
          validationPassedData, failedProductDetail, false);
      bulkCncUpsertErrorDTOList
          .add(new BulkCncUpsertErrorDTO(failedProductDetail.get(BulkParameters.BLIBLI_SKU),
              failedProductDetail.get(BulkParameters.PICKUP_POINT_CODE), errorMessage));
    }
  }

  public static String constructErrorMessageCampaign(String itemSKU, String ppCode,
    String errorReason) {
    StringBuilder errorMessage =
      new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(BulkUpdateServiceUtil.IS)
        .append(itemSKU).append(BulkUpdateServiceUtil.SPACE).append(BulkParameters.PICKUP_POINT_CODE)
        .append(BulkUpdateServiceUtil.IS).append(ppCode)
        .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILURE_REASON)
        .append(BulkUpdateServiceUtil.IS).append(errorReason);
    return errorMessage.toString();
  }

  private static String constructErrorMessageAndValidatePassedDataForBulkUpload(
      List<Map<String, String>> validationPassedData, Map<String, String> failedProductDetail,
    boolean isCampaign) {
    String errorMessage;
    List<Map<String, String>> invalidateData;
    if (failedProductDetail.containsKey(BulkParameters.ERROR_CODE)
        && CollectionUtils.isNotEmpty(validationPassedData)) {
      if (StringUtils.equals(failedProductDetail.get(BulkParameters.ERROR_CODE),
          BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.name())) {
        errorMessage = constructErrorMessage(failedProductDetail.get(BulkParameters.BLIBLI_SKU),
            BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
      } else {
        errorMessage = constructErrorMessage(failedProductDetail.get(BulkParameters.BLIBLI_SKU),
            failedProductDetail.get(BulkParameters.ERROR_CODE));
      }
      if (isCampaign){
        invalidateData = validationPassedData.stream().filter(
            e -> Optional.ofNullable(e.get(BulkParameters.ITEM_SKU_PP_CODE))
              .orElseGet(() -> e.get(BulkParameters.BLIBLI_SKU_WITH_EXAMPLE))
              .equals(failedProductDetail.get(BulkParameters.ITEM_SKU_PP_CODE)))
          .collect(Collectors.toList());
        errorMessage =
          constructErrorMessageCampaign(failedProductDetail.get(BulkParameters.BLIBLI_SKU),
            failedProductDetail.get(BulkParameters.PICKUP_POINT_CODE),
              failedProductDetail.get(BulkParameters.ERROR_CODE));
      }
      else {
        invalidateData = validationPassedData.stream().filter(
            e -> Optional.ofNullable(e.get(BulkParameters.BLIBLI_SKU)).orElseGet(() -> e.get(BulkParameters.BLIBLI_SKU_WITH_EXAMPLE))
              .equals(failedProductDetail.get(BulkParameters.BLIBLI_SKU)))
          .collect(Collectors.toList());
      }
      validationPassedData.removeAll(invalidateData);
    } else {
      if (StringUtils.isNotBlank(failedProductDetail.get(BulkParameters.PICKUP_POINT_CODE))) {
        errorMessage = constructErrorMessageForCncBulkUpload(
            failedProductDetail.get(BulkParameters.BLIBLI_SKU),
            failedProductDetail.get(BulkParameters.PICKUP_POINT_CODE), INTERNAL_ERROR);
      } else {
        errorMessage = constructErrorMessage(failedProductDetail.get(BulkParameters.BLIBLI_SKU),
            INTERNAL_ERROR);
      }
    }
    return errorMessage;
  }

  public static void addErrorCounterAndConstructErrorMessageForCncBulkDelete(
      List<Map<String, String>> failedInstantPickupProducts,
      List<BulkCncUpsertErrorDTO> bulkCncDeleteErrorDTOList,
      BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    for (Map<String, String> failedInstantPickupProduct : failedInstantPickupProducts) {
      bulkUpdateErrorCounter.incrementSystemErrorCounter();
      String errorMessage = constructErrorMessageForCncBulkUpload(
          failedInstantPickupProduct.get(BulkParameters.BLIBLI_SKU),
          failedInstantPickupProduct.get(BulkParameters.PICKUP_POINT_CODE),
          failedInstantPickupProduct.get(BulkParameters.ERROR_CODE));
      bulkCncDeleteErrorDTOList.add(
          new BulkCncUpsertErrorDTO(failedInstantPickupProduct.get(BulkParameters.BLIBLI_SKU),
              failedInstantPickupProduct.get(BulkParameters.PICKUP_POINT_CODE), errorMessage));
    }
  }

  public static void removeDirectory(String bulkProcessCode) {
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + bulkProcessCode;
    try {
      ProcessorUtils.deleteFile(directoryPath);
    } catch (Exception e) {
      LOGGER.error("Error while deleting the directories for Bulk Update. Directory path: {}",
          directoryPath, e);
    }
  }

  public static void removeDirectory(String bulkProcessType, String bulkProcessCode) {
    String directoryPath = StringUtils.EMPTY;
    switch (BulkProcessType.getBulkProcessType(bulkProcessType)) {
      case CAMPAIGN:
        directoryPath = ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkProcessCode;
        break;
      case PRODUCT_LEVEL_3:
      case ARCHIVE:
        directoryPath = ProcessorUtils.BULK_UPDATE_DIR + bulkProcessCode;
        break;
      case PRODUCT_CREATION_UPLOAD:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
      case CONVERTED_PRODUCT_CREATION_UPLOAD: {
        directoryPath = ProcessorUtils.DATA_BASE_DIR + bulkProcessCode;
        break;
      }
      case SUBJECT_TO_VAT:
        directoryPath = ProcessorUtils.BULK_VAT_UPDATE_DIR + bulkProcessCode;
        break;
    }
    try {
      ProcessorUtils.deleteFile(directoryPath);
    } catch (Exception e) {
      LOGGER.error("Error while deleting the directories for Bulk Update. Directory path: {}", directoryPath, e);
    }
  }

  private boolean validateDatasForBooleanHeaders(Map<String, String> cleanData,
      StringBuilder tempErrorMessage, BulkUpdateErrorCounter counter) {
    boolean isValueInvalid = false;
    boolean successFlag = true;
    double booleanHeadeValue = 0;
    for (Map.Entry<String, String> booleanValuesMap : HEADER_BOOLEAN_VALUES.entrySet()) {
      String booleanHeader = booleanValuesMap.getKey();
      if (!cleanData.containsKey(booleanHeader)) {
        continue;
      }
      isValueInvalid = isBooleanHeaderInvalid(cleanData, isValueInvalid, booleanHeader);
      if (isValueInvalid) {
        successFlag = false;
        counter.incrementBooleanHeader();
        if(counter.getBooleanHeaderCounter() <= ERROR_COUNT) {
          tempErrorMessage.append(booleanValuesMap.getValue()).append(AND_SYMBOL);
        }
        isValueInvalid = false;
      }
    }
    return successFlag;
  }

  public static boolean isBooleanHeaderInvalid(Map<String, String> cleanData, boolean isValueInvalid,
      String booleanHeader) {
    double booleanHeadeValue;
    try {
      booleanHeadeValue = Double.parseDouble(cleanData.get(booleanHeader));
      if (booleanHeadeValue != 1 && booleanHeadeValue != 0) {
        isValueInvalid = true;
      }
    } catch (Exception e) {
      isValueInvalid = true;
    }
    return isValueInvalid;
  }

  public static boolean validateDatasForPrice(Map<String, String> cleanData, StringBuilder tempErrorMessage,
      double regularPrice, double sellingPrice, BulkUpdateErrorCounter counter, int minimumPrice) {
    boolean isSellingPriceFormatValid = false;
    boolean isRegularPriceFormatValid = false;
    boolean successFlag = true;
    if (cleanData.containsKey(BulkParameters.PRICE_HEADER)) {
      try {
        regularPrice = Double.parseDouble(cleanData.get(BulkParameters.PRICE_HEADER));
        if (Double.compare(regularPrice, Double.NaN) == 0) {
          throw new ApplicationRuntimeException();
        }
        isRegularPriceFormatValid = true;
      } catch (Exception e) {
        successFlag = false;
        counter.incrementHargaCounter();
        if(counter.getHargaCounter() <= BulkUpdateServiceUtil.ERROR_COUNT) {
          tempErrorMessage.append(REGULAR_PRICE_MUST_BE_DECIMAL).append(AND_SYMBOL);
        }
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
            cleanData.get(BulkParameters.BLIBLI_SKU), REGULAR_PRICE_MUST_BE_DECIMAL);
      }
    }
    if (cleanData.containsKey(BulkParameters.SELLING_PRICE_HEADER)) {
      try {
        sellingPrice = Double.parseDouble(cleanData.get(BulkParameters.SELLING_PRICE_HEADER));
        if (Double.compare(sellingPrice, Double.NaN) == 0) {
          throw new ApplicationRuntimeException();
        }
        isSellingPriceFormatValid = true;
      } catch (Exception e) {
        successFlag = false;
        counter.incrementHargaPenjualanCounter();
        if(counter.getHargaPenjualanCounter() <= ERROR_COUNT) {
          tempErrorMessage.append(SELLING_PRICE_MUST_BE_DECIMAL).append(AND_SYMBOL);
        }
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
            cleanData.get(BulkParameters.BLIBLI_SKU), SELLING_PRICE_MUST_BE_DECIMAL);
      }
    }
    if (isRegularPriceFormatValid && isSellingPriceFormatValid) {
      if (regularPrice < minimumPrice ){
        successFlag = false;
        counter.incrementHargaCounter();
        if(counter.getHargaCounter() <= ERROR_COUNT){
          tempErrorMessage.append(PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE + minimumPrice)
              .append(AND_SYMBOL);
        }
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
            cleanData.get(BulkParameters.BLIBLI_SKU),
            PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE + minimumPrice);
      }
      if(sellingPrice < minimumPrice) {
        successFlag = false;
        counter.incrementHargaPenjualanCounter();
        if (counter.getHargaPenjualanCounter() <= ERROR_COUNT) {
          tempErrorMessage.append(PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE +  minimumPrice)
              .append(AND_SYMBOL);
        }
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
            cleanData.get(BulkParameters.BLIBLI_SKU),
            PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE + minimumPrice);
      }
      if (regularPrice < sellingPrice) {
        successFlag = false;
        counter.incrementHargaCounter();
        if(counter.getHargaCounter() <= ERROR_COUNT) {
          tempErrorMessage.append(PRICE_MUST_BE_GREATER_THAN_SELLING_PRICE).append(AND_SYMBOL);
        }
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
            cleanData.get(BulkParameters.BLIBLI_SKU), PRICE_MUST_BE_GREATER_THAN_SELLING_PRICE);
      }
    }
    return successFlag;
  }

  public boolean validateDataForBfbFields(Map<String, String> cleanData,
    StringBuilder tempErrorMessage, int minimumPrice) {
    boolean successFlag = true;
    double bfbPrice;
    if (cleanData.containsKey(BulkParameters.BFB_BASE_PRICE)) {
      try {
        bfbPrice = Double.parseDouble(cleanData.get(BulkParameters.BFB_BASE_PRICE));
        if (Double.compare(bfbPrice, Double.NaN) == 0) {
          throw new ApplicationRuntimeException();
        }
        if (bfbPrice < minimumPrice) {
          String sku = cleanData.get(BulkParameters.BLIBLI_SKU);
          tempErrorMessage.append(BFB_PRICE_MUST_BE_GREATER_THAN_ZERO).append(AND_SYMBOL);
          successFlag = false;
          LOGGER.error("Validation error : BlibliSku {} - error msg - {}", sku,
            String.format(BFB_PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE, minimumPrice));
        }
      } catch (Exception e) {
        successFlag = false;
        tempErrorMessage.append(BFB_PRICE_MUST_BE_NUMBER).append(AND_SYMBOL);
        LOGGER.error("Validation error : BlibliSku {} - error msg - {}",
          cleanData.get(BulkParameters.BLIBLI_SKU), REGULAR_PRICE_MUST_BE_DECIMAL);
      }
    }
    return successFlag;
  }

  public Integer prepareUpdateSummaryRequest(
      ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest,
      Map<String, String> productData, Map<String, Boolean> privilegedMap,
      ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    productLevel3UpdateSummaryRequest.setProductName(productData.get(BulkParameters.PRODUCT_NAME));
    productLevel3UpdateSummaryRequest.setMerchantSku(productData.get(BulkParameters.SELLER_SKU));
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false)) {
      productLevel3UpdateSummaryRequest
          .setPickupPointCode(productData.get(BulkParameters.PICKUP_POINT_HEADER));
    } else {
      productLevel3UpdateSummaryRequest
          .setPickupPointCode(productLevel3SummaryResponse.getPickupPointCode());
    }
    productLevel3UpdateSummaryRequest.setOff2OnActiveFlag(productLevel3SummaryResponse.getOff2OnActiveFlag());
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_STOCK, false)) {
      if (productLevel3SummaryResponse.getSynchronizeStock() == Boolean.TRUE) {
        productLevel3UpdateSummaryRequest.setDeltaStock(0);
      } else {
        String stockStr = productData.get(BulkParameters.STOCK_HEADER);
        if (StringUtils.isBlank(stockStr)) {
          stockStr = "0";
        }
        Double stock = Double.parseDouble(stockStr);
        productLevel3UpdateSummaryRequest.setDeltaStock(
            stock.intValue() - productLevel3SummaryResponse.getAvailableStockLevel2());
      }
    } else {
      productLevel3UpdateSummaryRequest.setDeltaStock(0);
    }
    setDisplayBuyable(privilegedMap, productData, productLevel3UpdateSummaryRequest,
        productLevel3SummaryResponse);
    Integer buyableFlagUpdatedCount =
        setRegularAndSellingPriceAndValidatedBuyableFlagUpdate(privilegedMap, productData,
            productLevel3UpdateSummaryRequest,
            productLevel3SummaryResponse);
    return buyableFlagUpdatedCount;
  }

  public Integer prepareUpdateL5SummaryRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, String> productL5Data, Map<String, Boolean> privilegedMap,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, boolean isMultiPickupPointSeller,
      boolean isPPCodeChangedForNonMppSeller,
      Map<String, ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseMap,
      Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet, ProfileResponse profileResponse, PreOrderDTO preOrderDTO) throws Exception {
    String cncStatus = productL5Data.get(BulkParameters.CNC_STATUS_HEADER);
    Integer buyableFlagUpdatedCount = 0;
    boolean isFbbPpCode = false;
    if (Objects.isNull(itemPickupPointListingL3Response) && isMultiPickupPointSeller) {
      if (isFbbTruePickupPoint(ImmutableSet.of(productL5Data.get(BulkParameters.PICKUP_POINT_HEADER)))
          && !mppForWhEnabled) {
        List<PickupPointDeleteRequest> pickupPointDeleteRequests = itemPickupPointListingL3ResponseMap.values().stream()
            .filter(ItemPickupPointListingL3Response::isFbbActivated)
            .map(repsonse -> new PickupPointDeleteRequest(repsonse.getPickupPointCode(), repsonse.getItemSku()))
            .collect(Collectors.toList());
        isFbbPpCode = true;
        if (CollectionUtils.isNotEmpty(pickupPointDeleteRequests)) {
          productVariantUpdateRequest.setDeletePickupPoints(pickupPointDeleteRequests);
        }
      }
      productVariantUpdateRequest.getAddPickupPoints().add(
          getAddItemPickupPointRequest(productL5Data, privilegedMap, itemPickupPointListingL3Response, cncStatus,
              isFbbPpCode, profileResponse, preOrderDTO, productVariantUpdateRequest));
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.ADD_PICKUP_POINT);
    }
    else {
      productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().add(
          getModifiedItemPickupPointRequest(productL5Data, cncStatus, privilegedMap, itemPickupPointListingL3Response,
              isPPCodeChangedForNonMppSeller, profileResponse, preOrderDTO));
    }
    return buyableFlagUpdatedCount;
  }

  private boolean isFbbTruePickupPoint(Set<String> pickupPointCodes) throws ApplicationException {
    Page<PickupPointResponse> pickupPointResponses =
        businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, pickupPointCodes.size(),
            PickupPointFilterRequest.builder().codes(pickupPointCodes).fbbActivated(true)
                .waitingDeletion(RequestHelper.getWaitingDeletion(setWaitingDeletionForDeletePickupPoint)).build());
    return !pickupPointResponses.isEmpty();
  }

  public ProductVariantPriceStockAndImagesRequest getProductVariantPriceStockAndImagesRequest(
      Map<String, String> productL5Data, ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      Map<String, Boolean> privilegedMap) {
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemSku(productL5Data.get(BulkParameters.BLIBLI_SKU));
    productVariantPriceStockAndImagesRequest.setSkuCode(itemPickupPointListingL3Response.getSkuCode());
    productVariantPriceStockAndImagesRequest.setMerchantSku(productL5Data.get(BulkParameters.SELLER_SKU));
    productVariantPriceStockAndImagesRequest.setItemName(itemPickupPointListingL3Response.getItemName());
    productVariantPriceStockAndImagesRequest.setUpcCode(itemPickupPointListingL3Response.getUpcCode());
    productVariantPriceStockAndImagesRequest.setFreeSample(itemPickupPointListingL3Response.isFreeSample());
    return productVariantPriceStockAndImagesRequest;
  }

  public ProductVariantPriceStockAndImagesRequest getProductVariantPriceStockAndImagesRequestFromUserInput(
      Map<String, String> productL5Data) {
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemSku(productL5Data.get(BulkParameters.BLIBLI_SKU));
    productVariantPriceStockAndImagesRequest.setSkuCode(productL5Data.get(BulkParameters.SKU_CODE_HEADER));
    productVariantPriceStockAndImagesRequest.setMerchantSku(productL5Data.get(BulkParameters.SELLER_SKU));
    productVariantPriceStockAndImagesRequest.setItemName(productL5Data.get(BulkParameters.NAMA_PRODUK));
    productVariantPriceStockAndImagesRequest.setUpcCode(null);
    productVariantPriceStockAndImagesRequest.setFreeSample(false);
    return productVariantPriceStockAndImagesRequest;
  }

  private ItemPickupPointRequest getAddItemPickupPointRequest(Map<String, String> productL5Data,
      Map<String, Boolean> privilegedMap, ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      String cncStatus, boolean isFbbPpCodeAdded, ProfileResponse profileResponse, PreOrderDTO preOrderDTO,
      ProductVariantUpdateRequest productVariantUpdateRequest) throws Exception {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(productL5Data.get(BulkParameters.BLIBLI_SKU));
    String productSku = productL5Data.get(BulkParameters.BLIBLI_PRODUCT_SKU);
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false)) {
      itemPickupPointRequest.setPickupPointId(productL5Data.get(BulkParameters.PICKUP_POINT_HEADER));
    } else {
      throw new ApplicationException(ErrorCategory.AUTHORIZATION,
          SELLER_NOT_AUTHORIZED_TO_EDIT + productL5Data.get(BulkParameters.PICKUP_POINT_HEADER));
    }
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_STOCK, false)) {
      if (preOrderQuotaFeatureSwitch && Objects.isNull(preOrderDTO)) {
        preOrderDTO = Optional.ofNullable(xProductOutboundService.getBasicProductInfo(profileResponse.getStoreId(),
            productSku)).map(BasicProductResponse::getPreOrder).orElse(null);
      }
      if (isFbbPpCodeAdded && CommonUtils.isNotFaasEligible(faasFeatureSwitch, profileResponse)) {
        itemPickupPointRequest.setStock(0);
      } else if (preOrderQuotaFeatureSwitch && CommonUtils.isOMGSeller(profileResponse) && CommonUtils.isPreOrderActive(
          preOrderDTO)) {
        itemPickupPointRequest.setStock(0);
        try {
          Double poQuota = Double.valueOf(productL5Data.get(BulkParameters.PO_QUOTA));
          itemPickupPointRequest.setInitialPreOrderQuota(poQuota.intValue());
          PreOrderRequest preOrderRequest = new PreOrderRequest();
          com.gdn.mta.bulk.service.util.BeanUtils.copyProperties(preOrderDTO, preOrderRequest);
          productVariantUpdateRequest.setPreOrder(preOrderRequest);
        } catch (NumberFormatException exception) {
          log.error("Invalid po quota value {}", productL5Data.get(BulkParameters.PO_QUOTA));
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
              BulkErrorCategory.PO_QUOTA_MUST_BE_NUMBER.getDescription());
        }
      }
      else {
        try {
          Double stock = Double.parseDouble(productL5Data.get(BulkParameters.STOCK_HEADER));
          itemPickupPointRequest.setStock(stock.intValue());
        } catch (NumberFormatException exception) {
          log.error("Invalid stock value {}", productL5Data.get(BulkParameters.STOCK_HEADER));
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
                  BulkErrorCategory.STOCK_MUST_BE_NUMBER.getDescription());
        }
      }
    } else {
      throw new ApplicationException(ErrorCategory.AUTHORIZATION,
          SELLER_NOT_AUTHORIZED_TO_EDIT + productL5Data.get(BulkParameters.STOCK_HEADER));
    }
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_PRICE, false)) {
      Double price = Double.parseDouble(productL5Data.get(BulkParameters.PRICE_HEADER));
      itemPickupPointRequest.setPrice(price);
      Double salesPrice = Double.parseDouble(productL5Data.get(BulkParameters.SELLING_PRICE_HEADER));
      itemPickupPointRequest.setSalePrice(salesPrice);
    } else {
      throw new ApplicationException(ErrorCategory.AUTHORIZATION,
          SELLER_NOT_AUTHORIZED_TO_EDIT + productL5Data.get(BulkParameters.PRICE_HEADER));
    }
    checkStatusCombination(privilegedMap, productL5Data, profileResponse.getCompany().isCncActivated());
    setDisplayBuyableL5CncForWarehouseOn(privilegedMap, productL5Data, itemPickupPointRequest,
        itemPickupPointListingL3Response, profileResponse.getCompany().isCncActivated());
    itemPickupPointRequest.setB2bFields(
      setB2BFieldsFromExcel(privilegedMap,productL5Data,itemPickupPointListingL3Response));
    if (cncForWarehouseFeatureSwitch) {
      setDisplayBuyableL5CncChannel(privilegedMap, productL5Data,
          itemPickupPointRequest, itemPickupPointListingL3Response);
    }
    itemPickupPointRequest.setSellerSku(productL5Data.get(BulkParameters.SELLER_SKU));
    if (StringUtils.isEmpty(productL5Data.get(BulkParameters.SELLER_SKU))) {
      setSellerSku(itemPickupPointRequest, productL5Data.get(BulkParameters.BLIBLI_SKU));
    }
    return itemPickupPointRequest;
  }

  public void checkStatusCombination(Map<String, Boolean> privilegedMap,
      Map<String, String> productL5Data, boolean isCncActivated) {
    if (!cncForWarehouseFeatureSwitch) {
      return;
    }

    if (Boolean.FALSE.equals(privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false))) {
      String amphiSkuStatus = productL5Data.get(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);
      String cncStatus = productL5Data.get(BulkParameters.CNC_STATUS_HEADER);
      String deliveryStatus = getDeliveryStatusBasedOnMerchantType(isCncActivated, productL5Data, amphiSkuStatus);
      if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, cncStatus) && StringUtils.equals(
          BulkParameters.OFFLINE_VALUE, deliveryStatus)) {
        if (!StringUtils.equals(BulkParameters.OFFLINE_VALUE, amphiSkuStatus)) {
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
              BulkErrorCategory.INVALID_STATUS_COMBINATION.getDescription());
        }
      }
    } else {
      String externalSkuStatus = productL5Data.get(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);
      String cncStatus = productL5Data.get(BulkParameters.CNC_STATUS_HEADER);
      String deliveryStatus = getDeliveryStatusBasedOnMerchantType(isCncActivated, productL5Data, externalSkuStatus);
      if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, cncStatus) && StringUtils.equals(
          BulkParameters.OFFLINE_VALUE, deliveryStatus)) {
        if (!StringUtils.equals(BulkParameters.OFFLINE_VALUE, externalSkuStatus)) {
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
              BulkErrorCategory.INVALID_STATUS_COMBINATION.getDescription());
        }
      }
    }
  }

  private String getDeliveryStatusBasedOnMerchantType (boolean isCncActivated, Map<String, String> productL5Data, String skuStatus) {
    if (isCncActivated) {
      return productL5Data.get(BulkParameters.DELIVERY_STATUS_HEADER);
    } else {
      return StringUtils.equals(BulkParameters.OFFLINE_VALUE, skuStatus) ?
          BulkParameters.OFFLINE_VALUE :
          BulkParameters.ONLINE_VALUE;
    }
  }

  private ItemPickupPointRequest setSellerSku(ItemPickupPointRequest itemPickupPointRequest, String itemSku) {
    List<ItemBasicDetailV2Response> responses = xProductOutboundService.getItemBasicDetailsByItemSku(itemSku);
    itemPickupPointRequest.setSellerSku(responses.stream().findFirst().get().getMerchantSku());
    return itemPickupPointRequest;
  }

  private ItemPickupPointRequest getModifiedItemPickupPointRequest(Map<String, String> productL5Data, String cncStatus,
      Map<String, Boolean> privilegedMap, ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      boolean isPPCodeChangedForNonMppSeller, ProfileResponse profileResponse, PreOrderDTO preOrderDTO) {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(productL5Data.get(BulkParameters.BLIBLI_SKU));
    itemPickupPointRequest.setSellerSku(productL5Data.get(BulkParameters.SELLER_SKU));
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false)) {
      itemPickupPointRequest.setPickupPointId(productL5Data.get(BulkParameters.PICKUP_POINT_HEADER));
    } else {
      itemPickupPointRequest.setPickupPointId(itemPickupPointListingL3Response.getPickupPointCode());
    }

    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_STOCK, false)) {
      if (preOrderQuotaFeatureSwitch && Objects.isNull(preOrderDTO)) {
        preOrderDTO = Optional.ofNullable(
                xProductOutboundService.getBasicProductInfo(profileResponse.getStoreId(),
                    itemPickupPointListingL3Response.getProductSku()))
            .map(BasicProductResponse::getPreOrder).orElse(null);
      }
      if (itemPickupPointListingL3Response.isWebSyncStock() == Boolean.TRUE && CommonUtils.isNotFaasEligible(
          faasFeatureSwitch, profileResponse)) {
        itemPickupPointRequest.setStock(0);
      } else if (preOrderQuotaFeatureSwitch && CommonUtils.isOMGSeller(profileResponse) && CommonUtils.isPreOrderActive(
          preOrderDTO)) {
        itemPickupPointRequest.setStock(0);
        int poQuota = parseStockValue(productL5Data, BulkParameters.PO_QUOTA);
        itemPickupPointRequest.setInitialPreOrderQuota(
            poQuota - Optional.ofNullable(itemPickupPointListingL3Response.getInitialPreOrderQuota())
                .orElseGet(() -> 0));
      } else {
        int stock = parseStockValue(productL5Data, BulkParameters.STOCK_HEADER);
        itemPickupPointRequest.setStock(stock - Optional.ofNullable(itemPickupPointListingL3Response.getAvailableStockLevel2()).orElse(0));
        String stockReminderStr = productL5Data.get(BulkParameters.STOCK_REMINDER_COLUMN_ID);
        if (StringUtils.isNotBlank(stockReminderStr)) {
          int minimumStock = parseStockValue(productL5Data, BulkParameters.STOCK_REMINDER_COLUMN_ID);
          itemPickupPointRequest.setMinimumStock(minimumStock);
        }
      }
    } else {
      itemPickupPointRequest.setStock(0);
    }
    checkStatusCombination(privilegedMap, productL5Data, profileResponse.getCompany().isCncActivated());
    setDisplayBuyableL5CncForWarehouseOn(privilegedMap, productL5Data, itemPickupPointRequest,
        itemPickupPointListingL3Response, profileResponse.getCompany().isCncActivated());
    itemPickupPointRequest.setB2bFields(
      setB2BFieldsFromExcel(privilegedMap, productL5Data, itemPickupPointListingL3Response));
    if (cncForWarehouseFeatureSwitch) {
      setDisplayBuyableL5CncChannel(privilegedMap, productL5Data,
          itemPickupPointRequest, itemPickupPointListingL3Response);
    }
    itemPickupPointRequest.setPpCodeChangedForNonMppSeller(isPPCodeChangedForNonMppSeller);
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_PRICE, false)) {
      Double price = Double.parseDouble(productL5Data.get(BulkParameters.PRICE_HEADER));
      itemPickupPointRequest.setPrice(price);
      Double salesPrice = Double.parseDouble(productL5Data.get(BulkParameters.SELLING_PRICE_HEADER));
      itemPickupPointRequest.setSalePrice(salesPrice);
    } else {
      itemPickupPointRequest.setPrice(itemPickupPointListingL3Response.getPrices().get(0).getPrice());
      itemPickupPointRequest.setSalePrice(itemPickupPointListingL3Response.getPrices().get(0).getSalePrice());
    }
    return itemPickupPointRequest;
  }

  private int parseStockValue(Map<String, String> productL5Data, String stockHeader) {
    String stockStr = productL5Data.get(stockHeader);
    try {
      return (int) Double.parseDouble(stockStr);
    } catch (NumberFormatException exception) {
      log.info("Invalid stock value {}", stockStr);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          BulkErrorCategory.STOCK_MUST_BE_NUMBER.getDescription());
    }
  }

  public void setDisplayBuyable(Map<String, Boolean> privilegedMap, Map<String, String> productData,
      ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest,
      ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false)) {
      if (!privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)) {
        String amphiSkuStatus = productData.get(BulkParameters.AMPHI_SKU_STATUS);
        if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, amphiSkuStatus)) {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setDisplay(Boolean.FALSE);
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setBuyable(Boolean.FALSE);
        } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, amphiSkuStatus)) {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setDisplay(Boolean.TRUE);
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setBuyable(Boolean.TRUE);
        } else if (StringUtils.equals(BulkParameters.TEASER_VALUE, amphiSkuStatus)) {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setDisplay(Boolean.TRUE);
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setBuyable(Boolean.FALSE);
        } else if (StringUtils.equals(BulkParameters.B2B_VALUE, amphiSkuStatus)) {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setDisplay(Boolean.FALSE);
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setBuyable(Boolean.TRUE);
        } else {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0)
              .setDisplay(productLevel3SummaryResponse.getViewConfigs().get(0).getDisplay());
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0)
              .setBuyable(productLevel3SummaryResponse.getViewConfigs().get(0).getBuyable());
        }
      } else {
        String ExternalSkuStatus = productData.get(BulkParameters.EXTERNAL_SKU_STATUS);
        if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, ExternalSkuStatus)) {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setDisplay(Boolean.FALSE);
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setBuyable(Boolean.FALSE);
        } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, ExternalSkuStatus)) {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setDisplay(Boolean.TRUE);
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0).setBuyable(Boolean.TRUE);
        } else {
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0)
              .setDisplay(productLevel3SummaryResponse.getViewConfigs().get(0).getDisplay());
          productLevel3UpdateSummaryRequest.getViewConfigs().get(0)
              .setBuyable(productLevel3SummaryResponse.getViewConfigs().get(0).getBuyable());
        }
      }
    } else {
      if (!privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)) {
        productLevel3UpdateSummaryRequest.getViewConfigs().get(0)
            .setDisplay(productLevel3SummaryResponse.getViewConfigs().get(0).getDisplay());
      }
      productLevel3UpdateSummaryRequest.getViewConfigs().get(0)
          .setBuyable(productLevel3SummaryResponse.getViewConfigs().get(0).getBuyable());
    }
  }

  public void setDisplayBuyableL5(Map<String, Boolean> privilegedMap, Map<String, String> productL5Data,
      ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse = null;
    if (Objects.nonNull(itemPickupPointListingL3Response)) {
      productLevel3DefaultViewConfigResponse =
        RequestHelper.getDefaultChannelItemViewConfig(itemPickupPointListingL3Response);
    }
    if (Boolean.TRUE.equals(privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false))) {
      if (Boolean.FALSE.equals(privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false))) {
        String amphiSkuStatus = productL5Data.get(BulkParameters.AMPHI_SKU_STATUS);
        if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, amphiSkuStatus)) {
          itemPickupPointRequest.setDisplay(Boolean.FALSE);
          itemPickupPointRequest.setBuyable(Boolean.FALSE);
        } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, amphiSkuStatus)) {
          if (Objects.nonNull(itemPickupPointListingL3Response) && Boolean.TRUE.equals(
            itemPickupPointListingL3Response.isFreeSample())) {
            itemPickupPointRequest.setDisplay(Boolean.FALSE);
            itemPickupPointRequest.setBuyable(Boolean.FALSE);
          } else {
            itemPickupPointRequest.setDisplay(Boolean.TRUE);
            itemPickupPointRequest.setBuyable(Boolean.TRUE);
          }
        } else if (StringUtils.equals(BulkParameters.TEASER_VALUE, amphiSkuStatus)) {
          itemPickupPointRequest.setDisplay(Boolean.TRUE);
          itemPickupPointRequest.setBuyable(Boolean.FALSE);
        } else if (StringUtils.equals(BulkParameters.B2B_VALUE, amphiSkuStatus)) {
          itemPickupPointRequest.setDisplay(Boolean.FALSE);
          itemPickupPointRequest.setBuyable(Boolean.TRUE);
        } else {
          if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
            itemPickupPointRequest.setDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
            itemPickupPointRequest.setBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
          } else {
            itemPickupPointRequest.setDisplay(Boolean.FALSE);
            itemPickupPointRequest.setBuyable(Boolean.FALSE);
          }
        }
      } else {
        String externalSkuStatus = productL5Data.get(BulkParameters.EXTERNAL_SKU_STATUS);
        if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, externalSkuStatus)) {
          itemPickupPointRequest.setDisplay(Boolean.FALSE);
          itemPickupPointRequest.setBuyable(Boolean.FALSE);
        } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, externalSkuStatus)) {
          itemPickupPointRequest.setDisplay(Boolean.TRUE);
          itemPickupPointRequest.setBuyable(Boolean.TRUE);
        } else {
          if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
            itemPickupPointRequest.setDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
            itemPickupPointRequest.setBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
          } else {
            itemPickupPointRequest.setDisplay(Boolean.FALSE);
            itemPickupPointRequest.setBuyable(Boolean.FALSE);
          }
        }
      }
    } else {
      itemPickupPointRequest.setDisplay(Boolean.FALSE);
      itemPickupPointRequest.setBuyable(Boolean.FALSE);
    }
  }

  public void setDisplayBuyableL5CncForWarehouseOn(Map<String, Boolean> privilegedMap,
      Map<String, String> productL5Data, ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, boolean isCncActivated) {
    ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse = null;
    if (Objects.nonNull(itemPickupPointListingL3Response)) {
      productLevel3DefaultViewConfigResponse =
          RequestHelper.getDefaultChannelItemViewConfig(itemPickupPointListingL3Response);
    }
    if (Boolean.TRUE.equals(privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false))) {
      if (Boolean.FALSE.equals(privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false))) {
        String status = cncForWarehouseFeatureSwitch ?
            BulkParameters.AMPHI_SKU_STATUS_CNC_1P :
            BulkParameters.AMPHI_SKU_STATUS;
        String amphiSkuStatus = productL5Data.get(status);
        String deliveryStatus = getDeliveryStatusBasedOnMerchantType(isCncActivated, productL5Data, amphiSkuStatus);
        if (!cncForWarehouseFeatureSwitch || StringUtils.equals(BulkParameters.ONLINE_VALUE,
            deliveryStatus)) {
          setBuyableDisplayableForAmphi(itemPickupPointRequest, itemPickupPointListingL3Response,
              amphiSkuStatus, productLevel3DefaultViewConfigResponse);
        }
      } else {
        String externalSkuStatus = cncForWarehouseFeatureSwitch ?
            productL5Data.get(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P) :
            productL5Data.get(BulkParameters.EXTERNAL_SKU_STATUS);
        String deliveryStatus = getDeliveryStatusBasedOnMerchantType(isCncActivated, productL5Data, externalSkuStatus);
        if (!cncForWarehouseFeatureSwitch || StringUtils.equals(BulkParameters.ONLINE_VALUE,
            deliveryStatus)) {
          setBuyableDisplayForExternal(itemPickupPointRequest, externalSkuStatus,
              productLevel3DefaultViewConfigResponse);
        }
      }
    } else {
      itemPickupPointRequest.setDisplay(Boolean.FALSE);
      itemPickupPointRequest.setBuyable(Boolean.FALSE);
    }
  }

  public void setDisplayBuyableL5CncChannel(Map<String, Boolean> privilegedMap,
      Map<String, String> productL5Data, ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse = null;
    if (Objects.nonNull(itemPickupPointListingL3Response)) {
      productLevel3DefaultViewConfigResponse =
          RequestHelper.getCncChannelItemViewConfig(itemPickupPointListingL3Response);
    }
    if (Boolean.TRUE.equals(privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false))) {
      if (Boolean.FALSE.equals(privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false))) {

        String amphiSkuStatus = productL5Data.get(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);
        String cncStatus = productL5Data.get(BulkParameters.CNC_STATUS_HEADER);
        if (StringUtils.equals(BulkParameters.ONLINE_VALUE, cncStatus)) {
          setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest,
              itemPickupPointListingL3Response, amphiSkuStatus,
              productLevel3DefaultViewConfigResponse);
        }
      } else {
        String externalSkuStatus = productL5Data.get(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);
        String cncStatus = productL5Data.get(BulkParameters.CNC_STATUS_HEADER);
        if (StringUtils.equals(BulkParameters.ONLINE_VALUE, cncStatus)) {
          setBuyableDisplayForExternalForCnc(itemPickupPointRequest, externalSkuStatus,
              productLevel3DefaultViewConfigResponse);
        }
      }
    } else {
      itemPickupPointRequest.setCncDisplay(Boolean.FALSE);
      itemPickupPointRequest.setCncBuyable(Boolean.FALSE);
    }
  }

  public void setBuyableDisplayForExternal(ItemPickupPointRequest itemPickupPointRequest,
      String externalSkuStatus,
      ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse) {
    if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, externalSkuStatus)) {
      itemPickupPointRequest.setDisplay(Boolean.FALSE);
      itemPickupPointRequest.setBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, externalSkuStatus)) {
      itemPickupPointRequest.setDisplay(Boolean.TRUE);
      itemPickupPointRequest.setBuyable(Boolean.TRUE);
    } else {
      if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
        itemPickupPointRequest.setDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
        itemPickupPointRequest.setBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
      } else {
        itemPickupPointRequest.setDisplay(Boolean.FALSE);
        itemPickupPointRequest.setBuyable(Boolean.FALSE);
      }
    }
  }

  public void setBuyableDisplayableForAmphi(ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, String amphiSkuStatus,
      ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse) {
    if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, amphiSkuStatus)) {
      itemPickupPointRequest.setDisplay(Boolean.FALSE);
      itemPickupPointRequest.setBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, amphiSkuStatus)) {
      if (Objects.nonNull(itemPickupPointListingL3Response) && Boolean.TRUE.equals(
          itemPickupPointListingL3Response.isFreeSample())) {
        itemPickupPointRequest.setDisplay(Boolean.FALSE);
        itemPickupPointRequest.setBuyable(Boolean.FALSE);
      } else {
        itemPickupPointRequest.setDisplay(Boolean.TRUE);
        itemPickupPointRequest.setBuyable(Boolean.TRUE);
      }
    } else if (StringUtils.equals(BulkParameters.TEASER_VALUE, amphiSkuStatus)) {
      itemPickupPointRequest.setDisplay(Boolean.TRUE);
      itemPickupPointRequest.setBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.B2B_VALUE, amphiSkuStatus)) {
      itemPickupPointRequest.setDisplay(Boolean.FALSE);
      itemPickupPointRequest.setBuyable(Boolean.TRUE);
    } else {
      if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
        itemPickupPointRequest.setDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
        itemPickupPointRequest.setBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
      } else {
        itemPickupPointRequest.setDisplay(Boolean.FALSE);
        itemPickupPointRequest.setBuyable(Boolean.FALSE);
      }
    }
  }

  public void setBuyableDisplayForExternalForCnc(ItemPickupPointRequest itemPickupPointRequest,
      String externalSkuStatus,
      ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse) {
    if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, externalSkuStatus)) {
      itemPickupPointRequest.setCncDisplay(Boolean.FALSE);
      itemPickupPointRequest.setCncBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, externalSkuStatus)) {
      itemPickupPointRequest.setCncDisplay(Boolean.TRUE);
      itemPickupPointRequest.setCncBuyable(Boolean.TRUE);
    } else {
      if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
        itemPickupPointRequest.setCncDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
        itemPickupPointRequest.setCncBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
      } else {
        itemPickupPointRequest.setCncDisplay(Boolean.FALSE);
        itemPickupPointRequest.setCncBuyable(Boolean.FALSE);
      }
    }
  }

  public void setBuyableDisplayableForAmphiForCnc(ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, String amphiSkuStatus,
      ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse) {
    if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, amphiSkuStatus)) {
      itemPickupPointRequest.setCncDisplay(Boolean.FALSE);
      itemPickupPointRequest.setCncBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, amphiSkuStatus)) {
      if (Objects.nonNull(itemPickupPointListingL3Response) && Boolean.TRUE.equals(
          itemPickupPointListingL3Response.isFreeSample())) {
        itemPickupPointRequest.setCncDisplay(Boolean.FALSE);
        itemPickupPointRequest.setCncBuyable(Boolean.FALSE);
      } else {
        itemPickupPointRequest.setCncDisplay(Boolean.TRUE);
        itemPickupPointRequest.setCncBuyable(Boolean.TRUE);
      }
    } else if (StringUtils.equals(BulkParameters.TEASER_VALUE, amphiSkuStatus)) {
      itemPickupPointRequest.setCncDisplay(Boolean.TRUE);
      itemPickupPointRequest.setCncBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.B2B_VALUE, amphiSkuStatus)) {
      itemPickupPointRequest.setCncDisplay(Boolean.FALSE);
      itemPickupPointRequest.setCncBuyable(Boolean.TRUE);
    } else {
      if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
        itemPickupPointRequest.setCncDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
        itemPickupPointRequest.setCncBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
      } else {
        itemPickupPointRequest.setCncDisplay(Boolean.FALSE);
        itemPickupPointRequest.setCncBuyable(Boolean.FALSE);
      }
    }
  }

  public B2BFields setB2BFieldsFromExcel(Map<String, Boolean> privilegedMap,
    Map<String, String> excelData,
    ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    B2BFields b2BFields = new B2BFields();
    ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse = null;
    if (Objects.nonNull(itemPickupPointListingL3Response)) {
      productLevel3DefaultViewConfigResponse =
        RequestHelper.getBfbChannelItemViewConfig(itemPickupPointListingL3Response);
    }
    if (Boolean.TRUE.equals(privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false))) {
      if (Boolean.TRUE.equals(privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false))) {
        String externalSkuStatus = excelData.get(BulkParameters.EXTERNAL_BFB_STATUS);
        setBfbFieldsForExternalUser(externalSkuStatus, b2BFields,
          productLevel3DefaultViewConfigResponse);
      } else {
        String amphiBfbAStatus = excelData.get(BulkParameters.AMPHI_BFB_STATUS);
        setBfbFieldsForAmphiUser(amphiBfbAStatus, b2BFields,
          productLevel3DefaultViewConfigResponse);
      }
    } else {
      b2BFields.setDisplay(Boolean.FALSE);
      b2BFields.setBuyable(Boolean.FALSE);
    }
    if (excelData.containsKey(BulkParameters.BFB_BASE_PRICE)) {
      b2BFields.setPrice(Double.parseDouble(excelData.get(BulkParameters.BFB_BASE_PRICE)));
    }
    if (StringUtils.isNotEmpty(excelData.get(BulkParameters.BFB_MANAGED))) {
      b2BFields.setManaged(BulkParameters.BFB_MANAGED_TRUE_VALUE.equals(
        Double.parseDouble(excelData.get(BulkParameters.BFB_MANAGED))) ?
        Boolean.TRUE :
        Boolean.FALSE);
    } else if (Objects.nonNull(itemPickupPointListingL3Response) && Objects.nonNull(
      itemPickupPointListingL3Response.getB2bFields())) {
      b2BFields.setManaged(itemPickupPointListingL3Response.getB2bFields().isManaged());
    } else {
      //default value if its a new pp code being added
      b2BFields.setManaged(false);
    }
    return b2BFields;
  }

  private void setBfbFieldsForExternalUser(String externalSkuStatus, B2BFields b2BFields,
    ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse) {
    if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, externalSkuStatus)) {
      b2BFields.setDisplay(Boolean.FALSE);
      b2BFields.setBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, externalSkuStatus)) {
      b2BFields.setDisplay(Boolean.TRUE);
      b2BFields.setBuyable(Boolean.TRUE);
    } else {
      if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
        b2BFields.setDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
        b2BFields.setBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
      } else {
        b2BFields.setDisplay(Boolean.FALSE);
        b2BFields.setBuyable(Boolean.FALSE);
      }
    }
  }

  private void setBfbFieldsForAmphiUser(String amphiBfbAStatus, B2BFields b2BFields,
    ProductLevel3ViewConfigResponse productLevel3DefaultViewConfigResponse) {
    if (StringUtils.equals(BulkParameters.OFFLINE_VALUE, amphiBfbAStatus)) {
      b2BFields.setDisplay(Boolean.FALSE);
      b2BFields.setBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.ONLINE_VALUE, amphiBfbAStatus)) {
      b2BFields.setDisplay(Boolean.TRUE);
      b2BFields.setBuyable(Boolean.TRUE);
    } else if (StringUtils.equals(BulkParameters.TEASER_VALUE, amphiBfbAStatus)) {
      b2BFields.setDisplay(Boolean.TRUE);
      b2BFields.setBuyable(Boolean.FALSE);
    } else if (StringUtils.equals(BulkParameters.B2B_VALUE, amphiBfbAStatus)) {
      b2BFields.setDisplay(Boolean.FALSE);
      b2BFields.setBuyable(Boolean.TRUE);
    } else {
      if (Objects.nonNull(productLevel3DefaultViewConfigResponse)) {
        b2BFields.setDisplay(productLevel3DefaultViewConfigResponse.getDisplay());
        b2BFields.setBuyable(productLevel3DefaultViewConfigResponse.getBuyable());
      } else {
        b2BFields.setDisplay(Boolean.FALSE);
        b2BFields.setBuyable(Boolean.FALSE);
      }
    }
  }

  public Integer setRegularAndSellingPriceAndValidatedBuyableFlagUpdate(Map<String, Boolean> privilegedMap,
      Map<String, String> productData,
      ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest,
      ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    Integer buyableFlagUpdatedCount = 0;
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_EDIT_PRICE, false)) {
      Double price = Double.parseDouble(productData.get(BulkParameters.PRICE_HEADER));
      productLevel3UpdateSummaryRequest.getPrices().get(0).setPrice(price);
      Double salesPrice = Double.parseDouble(productData.get(BulkParameters.SELLING_PRICE_HEADER));
      productLevel3UpdateSummaryRequest.getPrices().get(0).setSalePrice(salesPrice);
    } else {
      productLevel3UpdateSummaryRequest.getPrices().get(0)
          .setPrice(productLevel3SummaryResponse.getPrices().get(0).getPrice());
      productLevel3UpdateSummaryRequest.getPrices().get(0)
          .setSalePrice(productLevel3SummaryResponse.getPrices().get(0).getSalePrice());
    }
    return buyableFlagUpdatedCount;
  }

  public static void mandatoryRequestParamValidation(MandatoryRequestParam mandatoryRequestParam) {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(mandatoryRequestParam.getStoreId()),
        GenericErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(mandatoryRequestParam.getChannelId()),
        GenericErrorMessages.CHANNEL_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(mandatoryRequestParam.getRequestId()),
        GenericErrorMessages.REQUEST_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(mandatoryRequestParam.getClientId()),
        GenericErrorMessages.CLIENT_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(mandatoryRequestParam.getUsername()),
        GenericErrorMessages.USERNAME_MUST_NOT_BE_BLANK);
  }

  public static void bulkProcessUpdateRequestValidation(BulkProcessUpdateRequest bulkRequest) {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(bulkRequest.getBulkProcessType()),
        GenericErrorMessages.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(bulkRequest.getFileName()),
        GenericErrorMessages.FILE_NAME_MUST_NOT_BE_BLANK);
  }

  public static byte[] deletePassedProductFromExcel(Sheet excelSheetData,
   List<Map<String, String>> validationPassedData,
      String bulkProcessCode, String filename, String baseDir) throws IOException {

    List<Integer> validRowNumberList = new ArrayList<>();
    for (Map<String, String> productRow : validationPassedData) {
      validRowNumberList.add(Integer.parseInt(productRow.get("RowNumber")));
    }

    Collections.sort(validRowNumberList, Collections.reverseOrder());
    return removeRowFromExcel(validRowNumberList, excelSheetData);
  }

  public static void deleteValidCampaignProductFromExcel(Sheet excelSheetData,
      List<Map<String, String>> validationPassedData, String bulkProcessCode, String filename)
      throws Exception {
    List<Integer> validRowNumberList = new ArrayList<>();
    for (Map<String, String> productRow : validationPassedData) {
      validRowNumberList.add(Integer.parseInt(productRow.get("RowNumber")));
    }
    validRowNumberList.sort(Collections.reverseOrder());
    byte[] bytes = removeRowFromExcel(validRowNumberList, excelSheetData);
    ProcessorUtils.createFile(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkProcessCode
        + File.separator + bulkProcessCode + ProcessorUtils.getFileFormat(filename), bytes);
  }

  public static void deletePassedProductFromUploadProductExcel(InputStream fileInputStream,
      List<Integer> validRowNumberList, String bulkProcessCode, String excelFileType) throws Exception {
    if (Constant.FILE_TYPE_XLS.equalsIgnoreCase(excelFileType)) {
      HSSFWorkbook workbook = new HSSFWorkbook(fileInputStream);
      HSSFSheet excelSheetData = workbook.getSheetAt(0);
      byte[] bytes = removeRowFromExcel(validRowNumberList, excelSheetData);
      ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + File.separator + bulkProcessCode
          + ProcessorUtils.FILETYPE_EXCEL, bytes);
      workbook.close();
    } else {
      XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
      XSSFSheet sheet = workbook.getSheetAt(0);
      byte[] bytes = removeRowFromExcel(validRowNumberList, sheet);
      ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + File.separator + bulkProcessCode
          + excelFileType, bytes);
      workbook.close();
    }
  }

  public static void deletePassedProductFromUploadGenericProductExcel(InputStream fileInputStream,
      List<Integer> validRowNumberList, String bulkProcessCode, int totalNumberOfRows, String excelFileType)
      throws Exception {
    if (Constant.FILE_TYPE_XLS.equalsIgnoreCase(excelFileType)) {
      HSSFWorkbook workbook = new HSSFWorkbook(fileInputStream);
      HSSFSheet excelSheetData = workbook.getSheetAt(0);
      byte[] bytes = removeRowFromExcel(validRowNumberList, excelSheetData);
      ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + File.separator + bulkProcessCode
          + ProcessorUtils.FILETYPE_EXCEL, bytes);
      workbook.close();
    } else {
      XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
      XSSFSheet sheet = workbook.getSheet(GenericBulkParameters.USER_INPUT_DATA_SHEET);
      byte[] bytes = removeRowFromGenericExcel(validRowNumberList, sheet, totalNumberOfRows);
      ProcessorUtils
          .createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + File.separator + bulkProcessCode + excelFileType,
              bytes);
      workbook.close();
    }
  }

  private static byte[] removeRowFromExcel(List<Integer> validRowNumberList, Sheet excelSheetData) throws IOException {
    for (Integer validRowNumber : validRowNumberList) {
      Integer totalsRows = excelSheetData.getLastRowNum();
      excelSheetData.removeRow(excelSheetData.getRow(validRowNumber));
      if (!Objects.equals(validRowNumber, totalsRows)) {
        excelSheetData.shiftRows(validRowNumber + 1, excelSheetData.getLastRowNum(), -1);
      }
    }
    return POIUtil.getByteContentFromExcel(excelSheetData);
  }

  private static byte[] removeRowFromGenericExcel(List<Integer> validRowNumberList, Sheet excelSheetData,
      int totalNumberOfRows) throws Exception {
    for (int validRow = 0; validRow < validRowNumberList.size(); validRow++) {
      Integer validRowNumber = validRowNumberList.get(validRow);
      // Here + GenericBulkParameters.HEADER_END_ROW because user input row starts from 14th row
      excelSheetData.removeRow(excelSheetData.getRow(validRowNumber + GenericBulkParameters.HEADER_END_ROW));
      if (!Objects.equals(validRowNumber + GenericBulkParameters.HEADER_END_ROW + 1,
          totalNumberOfRows + GenericBulkParameters.HEADER_END_ROW - validRow)) {
        // Here - validRow because after removeRow in the above line, the total number of rows will
        // get reduced in every iteration
        excelSheetData.shiftRows(validRowNumber + GenericBulkParameters.HEADER_END_ROW + 1,
            totalNumberOfRows + GenericBulkParameters.HEADER_END_ROW - validRow, -1);
      }
    }
    return POIUtil.getByteContentFromExcel(excelSheetData);
  }

  @Transactional
  public BulkProcess savePreProcessBulkData(BulkProcess bulkProcess) {
    return bulkProcessRepository.save(bulkProcess);
  }

  private static void validateDuplicateSku(List<Map<String, String>> cleanDataList,
      List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList,
      BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    Set<String> validSkuList = new HashSet<>();
    Set<String> invalidSkuList = new HashSet<>();
    cleanDataList.forEach(cleanData -> {
      if (validSkuList.contains(cleanData.get(BulkParameters.BLIBLI_SKU))) {
        invalidSkuList.add(cleanData.get(BulkParameters.BLIBLI_SKU));
      }
      validSkuList.add(cleanData.get(BulkParameters.BLIBLI_SKU));
    });
    updateErrorDTOList(cleanDataList, invalidSkuList, bulkUpdateErrorDTOList,
        bulkUpdateErrorCounter);
  }

  //validating row's for duplicate sku and ppcode
  private static void validateDuplicateItemSkuAndPPCode(List<Map<String, String>> cleanDataList,
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList,
    BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    Set<String> validSkuPPList = new HashSet<>();
    Set<String> invalidSkuPPList = new HashSet<>();
    cleanDataList.forEach(cleanData -> {
      if (validSkuPPList.contains(cleanData.get(BulkParameters.BLIBLI_SKU) + Constant.DASH + cleanData
        .get(BulkParameters.PICKUP_POINT_CODE))) {
        invalidSkuPPList.add(cleanData.get(BulkParameters.BLIBLI_SKU) + Constant.DASH + cleanData
          .get(BulkParameters.PICKUP_POINT_CODE));
      }
      validSkuPPList.add(cleanData.get(BulkParameters.BLIBLI_SKU) + Constant.DASH + cleanData
        .get(BulkParameters.PICKUP_POINT_CODE));
    });
    updateErrorDTOListForMppProduct(cleanDataList, invalidSkuPPList, bulkUpdateErrorDTOList,
      bulkUpdateErrorCounter);
  }

  /**
   * validate prices from product response when campaign response does not exist
   * @param bulkUpdateServiceValidation
   * @param itemSkuIndex
   * @param itemSku
   * @return true or false
   */
  private boolean isPriceValidatedWhenCampaignEqualsNull(BulkUpdateServiceValidation bulkUpdateServiceValidation,
      int itemSkuIndex, String itemSku) {

    return this.isProductIncludedInCampaign(
        bulkUpdateServiceValidation.getBulkAddCampaignProductQueue(),
        bulkUpdateServiceValidation.getTempErrorMessage(),
        bulkUpdateServiceValidation.getProductLevel3SummaryResponseMap().get(itemSku))
        && this.isPriceCutGreaterThanSellingPrice(
        bulkUpdateServiceValidation.getTempErrorMessage(),
        bulkUpdateServiceValidation.getTempCleanDataList().get(itemSkuIndex),
        bulkUpdateServiceValidation.getProductLevel3SummaryResponseMap().get(itemSku));
  }

  /**
   * validate quota and prices when both campaign and product response exist
   * @param bulkUpdateServiceValidation
   * @param itemSkuIndex
   * @param itemSku
   * @return true or false
   */
  private boolean isQuotaAndPriceValidated(BulkUpdateServiceValidation bulkUpdateServiceValidation,
      int itemSkuIndex, String itemSku) {

    return this.isInputQuotaGreaterThanUsedQuota(
        bulkUpdateServiceValidation.getTempErrorMessage(),
        bulkUpdateServiceValidation.getTempCleanDataList().get(itemSkuIndex),
        bulkUpdateServiceValidation.getCampaignProductDetailResponseMap().get(itemSku))
        && this.isProductIncludedInCampaign(bulkUpdateServiceValidation.getBulkAddCampaignProductQueue(),
        bulkUpdateServiceValidation.getTempErrorMessage(),
        bulkUpdateServiceValidation.getProductLevel3SummaryResponseMap().get(itemSku));
  }

  /**
   * assigning parameter from pbp to be used by campaignRequest later
   * @param tempCleanDataList
   * @param itemSkuIndex
   * @param productLevel3SummaryResponse
   */
  private void assignPbpParamsForCampaignRequest(List<Map<String, String>> tempCleanDataList,
      int itemSkuIndex, ProductLevel3SummaryResponse productLevel3SummaryResponse){
    Map<String, String> tempCleanData = tempCleanDataList.get(itemSkuIndex);
    tempCleanData.put(BulkParameters.CATEGORY, productLevel3SummaryResponse.getCategoryCode());
    tempCleanData.put(BulkParameters.PRODUCT_SKU, productLevel3SummaryResponse.getProductSku());
    tempCleanData.put(BulkParameters.SKU_NAME, productLevel3SummaryResponse.getItemName());
    tempCleanData.put(BulkParameters.NAMA_PRODUK, productLevel3SummaryResponse.getProductName());
    tempCleanData.put(BulkParameters.PRODUCT_CODE, productLevel3SummaryResponse.getProductCode());
    tempCleanData.put(BulkParameters.MERCHANT_CODE, productLevel3SummaryResponse.getMerchantCode());
    tempCleanData.put(BulkParameters.SALE_PRICE,
        productLevel3SummaryResponse.getPrices().get(0).getSalePrice().toString());
    tempCleanData.put(BulkParameters.NORMAL_SELLING_PRICE,
        String.valueOf(productLevel3SummaryResponse.getOriginalSellingPrice()));
    tempCleanDataList.add(tempCleanData);
  }

  public static String constructErrorMessage (String itemSKU ,String errorReason){
    StringBuilder errorMessage = new StringBuilder().append(BulkParameters.BLIBLI_SKU)
        .append(BulkUpdateServiceUtil.IS)
        .append(itemSKU)
        .append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.FAILURE_REASON)
        .append(BulkUpdateServiceUtil.IS)
        .append(errorReason);

    return errorMessage.toString();
  }

  private static String constructErrorMessageForCncBulkUpload(String itemSku,
      String pickupPointCode, String reason) {
    return new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(BulkUpdateServiceUtil.IS)
        .append(itemSku).append(BulkUpdateServiceUtil.AND_SYMBOL)
        .append(BulkParameters.PICKUP_POINT_CODE).append(BulkUpdateServiceUtil.IS)
        .append(pickupPointCode).append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.FAILURE_REASON).append(BulkUpdateServiceUtil.IS)
        .append(reason).toString();
  }

  public boolean authorizeUploadInstantPickupProductBulkUpsert(Sheet workSheet,
      BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter,
      Map<Integer, String> updatedHeaders)
      throws IOException {
    Map<Integer, String> headers = POIUtil.readHeadersFromCncExcel(workSheet);
    if (headers.isEmpty()) {
      saveBulkProcessAsAborted(bulkProcess, bulkUpdateQueue, counter);
      return false;
    }
    return validateForHeaderMismatchAndGetUpdatedHeaders(updatedHeaders, headers);
  }

  public void saveBulkProcessAsAborted(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue,
      BulkUpdateErrorCounter counter) {
    LOGGER.error("No data found in file for bulk upsert instant pickup product - bulkUpdateQueue: {}", bulkUpdateQueue);
    updateBulkStatusAborted(bulkProcess, bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(),
        FILE_BLANK_ERROR, counter);
    this.bulkProcessService.saveOperation(bulkProcess);
  }

  public boolean validateForHeaderMismatchAndGetUpdatedHeaders(Map<Integer, String> updatedHeaders,
      Map<Integer, String> headers) {
    int totalFieldsAuthorized = 0;
    if (headers.containsValue(ExcelHeaderNames.BLIBLI_SKU_IN) || headers.containsValue(ExcelHeaderNames.BLIBLI_SKU_EN)
        || headers.containsValue(BulkParameters.BLIBLI_SKU) || headers.containsValue(
        BulkParameters.BLIBLI_SKU_WITH_EXAMPLE)) {
      updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.BLIBLI_SKU_EN);
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(ExcelHeaderNames.PICKUP_POINT_CODE_IN) || headers.containsValue(
        ExcelHeaderNames.PICKUP_POINT_CODE_EN) || headers.containsValue(BulkParameters.PICKUP_POINT_CODE)) {
      updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.PICKUP_POINT_CODE_EN);
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(ExcelHeaderNames.LIST_PRICE_IN) || headers.containsValue(ExcelHeaderNames.LIST_PRICE_EN)
        || headers.containsValue(BulkParameters.LIST_PRICE)) {
      updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.LIST_PRICE_EN);
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(ExcelHeaderNames.OFFER_PRICE_IN) || headers.containsValue(ExcelHeaderNames.OFFER_PRICE_EN)
        || headers.containsValue(BulkParameters.OFFER_PRICE)) {
      updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.OFFER_PRICE_EN);
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(ExcelHeaderNames.STOCK_IN) || headers.containsValue(ExcelHeaderNames.STOCK_EN)) {
      updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.STOCK_EN);
      totalFieldsAuthorized++;
    }
    if (cncForWarehouseFeatureSwitch) {
      if (headers.containsValue(ExcelHeaderNames.DELIVERY_STATUS_IN_CNC_1P)
          || headers.containsValue(ExcelHeaderNames.DELIVERY_STATUS_EN_CNC_1P)) {
        updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.DELIVERY_STATUS_EN_CNC_1P);
        totalFieldsAuthorized++;
      }
    } else {
      if (headers.containsValue(ExcelHeaderNames.DELIVERY_STATUS_IN) || headers.containsValue(
          ExcelHeaderNames.DELIVERY_STATUS_EN)) {
        updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.DELIVERY_STATUS_EN);
        totalFieldsAuthorized++;
      }
    }
    if (headers.containsValue(ExcelHeaderNames.CNC_STATUS_EN)) {
      updatedHeaders.put(totalFieldsAuthorized, ExcelHeaderNames.CNC_STATUS_EN);
      totalFieldsAuthorized++;
    }
    return headers.size() == totalFieldsAuthorized;
  }

  public static List<BulkCncUpsertErrorDTO> validateExcelDataForBulkUpsertInstantPickupProduct(
      List<Map<String, String>> cleanData, List<Map<String, String>> successData,
      List<Map<String, String>> failureData, BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList = new ArrayList<>();

    if (CollectionUtils.isEmpty(cleanData)) {
      return bulkCncUpsertErrorDTOList;
    }

    for (Map<String, String> rowCleanData : cleanData) {
      StringBuilder tempErrorMessage =
          new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(IS).append(
                  Optional.ofNullable(rowCleanData.get(BulkParameters.BLIBLI_SKU))
                      .orElseGet(() -> rowCleanData.get(BulkParameters.BLIBLI_SKU_WITH_EXAMPLE)))
              .append(END_SYMBOL).append(FAILURE_REASON).append(IS);

      boolean successFlag = checkBlibliSKU(rowCleanData, tempErrorMessage);
      successFlag =
        checkPickupPointNotEmpty(rowCleanData, successFlag, BulkParameters.PICKUP_POINT_CODE,
          PICKUP_POINT_EMPTY, tempErrorMessage);
      successFlag = checkFieldValueMustBeNumberAndGreaterThanZero(rowCleanData, successFlag,
          tempErrorMessage, BulkParameters.LIST_PRICE);
      successFlag = checkFieldValueMustBeNumberAndGreaterThanZero(rowCleanData, successFlag,
          tempErrorMessage, BulkParameters.OFFER_PRICE);
      successFlag = checkFieldValueMustBeNumberAndPositive(rowCleanData, successFlag,
          tempErrorMessage, BulkParameters.STOCK);
      successFlag =
        checkPickupPointNotEmpty(rowCleanData, successFlag, BulkParameters.PICKUP_POINT_CODE,
          PICKUP_POINT_EMPTY, tempErrorMessage);
      validateAndAddSuccessAndFailedDataForCncBulkUpload(tempErrorMessage, successFlag,
          rowCleanData, successData, failureData, bulkCncUpsertErrorDTOList,
          bulkUpdateErrorCounter);
    }
    return bulkCncUpsertErrorDTOList;
  }

  public boolean authorizeUploadInstantPickupProductBulkDelete(Sheet workSheet,
      BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, BulkUpdateErrorCounter counter)
      throws IOException {
    Map<Integer, String> headers = POIUtil.readHeadersFromCncExcel(workSheet);
    if (headers.size() == 0) {
      LOGGER.error(
          "No data found in file for bulk delete instant pickup product - bulkUpdateQueue: {}",
          bulkUpdateQueue);
      updateBulkStatusAborted(bulkProcess, bulkUpdateQueue.getStoreId(),
          bulkUpdateQueue.getBulkProcessCode(), FILE_BLANK_ERROR, counter);
      this.bulkProcessService.saveOperation(bulkProcess);
      return false;
    }

    int totalFieldsAuthorized = 0;
    if (headers.containsValue(BulkParameters.BLIBLI_SKU)) {
      totalFieldsAuthorized++;
    }
    if (headers.containsValue(BulkParameters.PICKUP_POINT_CODE)) {
      totalFieldsAuthorized++;
    }
    return CNC_DELETE_UPLOAD_HEADERS_SIZE == totalFieldsAuthorized;
  }

  public static List<BulkCncUpsertErrorDTO> validateExcelDataForBulkDeleteInstantPickupProduct(
      List<Map<String, String>> cleanData, List<Map<String, String>> successData,
      List<Map<String, String>> failureData, BulkUpdateErrorCounter bulkUpdateErrorCounter) {
    List<BulkCncUpsertErrorDTO> bulkCncDeleteErrorDTOList = new ArrayList<>();

    if (CollectionUtils.isEmpty(cleanData)) {
      return bulkCncDeleteErrorDTOList;
    }

    for (Map<String, String> rowCleanData : cleanData) {
      StringBuilder tempErrorMessage = new StringBuilder().append(BulkParameters.BLIBLI_SKU)
          .append(IS).append(rowCleanData.get(BulkParameters.BLIBLI_SKU)).append(END_SYMBOL)
          .append(FAILURE_REASON).append(IS);
      boolean successFlag = checkBlibliSKU(rowCleanData, tempErrorMessage);
      validateAndAddSuccessAndFailedDataForCncBulkUpload(tempErrorMessage, successFlag,
          rowCleanData, successData, failureData, bulkCncDeleteErrorDTOList,
          bulkUpdateErrorCounter);
    }
    return bulkCncDeleteErrorDTOList;
  }

  public static void deletePassedDataFromExcel(Sheet excelSheetData,
      List<Map<String, String>> validationPassedData, String bulkProcessCode, String fileName, String directoryName)
      throws Exception {
    List<Integer> validRowNumberList = new ArrayList<>();
    for (Map<String, String> dataRow : validationPassedData) {
      validRowNumberList.add(Integer.parseInt(dataRow.get("RowNumber")));
    }
    validRowNumberList.sort(Collections.reverseOrder());
    byte[] bytes = removeRowFromExcel(validRowNumberList, excelSheetData);
    ProcessorUtils
        .createFile(directoryName + bulkProcessCode
            + File.separator + bulkProcessCode + ProcessorUtils.getFileFormat(fileName), bytes);
  }

  /**
   * construct bulk process note that consist of number of success and failed product
   */
  public void updateBulkProductFinalStatus(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue,
      int savedProduct, int totalProduct, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      BulkUpdateErrorCounter counter, List<String> descriptions) {
    setBulkProcessFinalData(bulkProcess, bulkUpdateQueue, savedProduct, totalProduct, descriptions);
    for (BulkUpdateErrorDTO bulkUpdateErrorDTO : listBulkUpdateErrorDTO) {
      setBulkProcessNotesAndErrorCount(bulkProcess, bulkUpdateQueue.getStoreId(),
          bulkUpdateQueue.getBulkProcessCode(), bulkUpdateErrorDTO.getReason(), counter);
    }
    LOGGER.info("Post processing for bulk update successfully completed. bulkUpdateQueue: {}",
        bulkUpdateQueue);
  }

  /**
   * construct bulk process note that consist of number of success and failed product for CnC bulk
   * upload feature
   */
  public void updateBulkCncProductFinalStatus(BulkProcess bulkProcess,
      BulkUpdateQueue bulkUpdateQueue, int savedProduct, int totalProduct,
      List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList,
      BulkUpdateErrorCounter bulkUpdateErrorCounter, List<String> descriptions) {
    setBulkProcessFinalData(bulkProcess, bulkUpdateQueue, savedProduct, totalProduct, descriptions);
    for (BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO : bulkCncUpsertErrorDTOList) {
      setBulkProcessNotesAndErrorCount(bulkProcess, bulkUpdateQueue.getStoreId(),
          bulkUpdateQueue.getBulkProcessCode(), bulkCncUpsertErrorDTO.getReason(),
          bulkUpdateErrorCounter);
    }
    LOGGER.info("Post processing for bulk update successfully completed. bulkUpdateQueue: {}",
        bulkUpdateQueue);
  }

  /**
   * construct bulk process note that consist of number of success and failed product for CnC bulk
   * delete feature
   */
  public void updateBulkDeleteCncProductFinalStatus(BulkProcess bulkProcess,
      BulkUpdateQueue bulkUpdateQueue, int savedProduct, int totalProduct,
      List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList,
      BulkUpdateErrorCounter bulkUpdateErrorCounter, List<String> descriptions) {
    setBulkProcessFinalData(bulkProcess, bulkUpdateQueue, savedProduct, totalProduct, descriptions);
    bulkProcess.setDescription(constructBulkDeleteProcessDescription(savedProduct, totalProduct));
    for (BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO : bulkCncUpsertErrorDTOList) {
      setBulkProcessNotesAndErrorCount(bulkProcess, bulkUpdateQueue.getStoreId(),
          bulkUpdateQueue.getBulkProcessCode(), bulkCncUpsertErrorDTO.getReason(),
          bulkUpdateErrorCounter);
    }
    LOGGER.info("Post processing for bulk delete successfully completed. bulkUpdateQueue: {}",
        bulkUpdateQueue);
  }

  private void setBulkProcessFinalData(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue,
      int savedProduct, int totalProduct, List<String> descriptions) {
    bulkProcess.setEndDate(new Date());
    bulkProcess.setDescription(
        constructBulkProcessDescription(bulkUpdateQueue, savedProduct, totalProduct, descriptions));
    bulkProcess.setSuccessCount(savedProduct);
    bulkProcess.setErrorCount(totalProduct - savedProduct);
  }

  private String constructBulkProcessDescription(BulkUpdateQueue bulkUpdateQueue, int savedProduct,
      int totalProduct, List<String> descriptions) {
    String description = MessageUtil.getMessage(DESCRIPTION_PRODUCT_BULK_PROCESS, BAHASA);
    description = description.replace("{0}", String.valueOf(descriptions.get(0)));
    description = description.replace("{1}", String.valueOf(bulkUpdateQueue.getFileName()));
    description = description.replace("{2}", String.valueOf(descriptions.get(1)));
    description = description.replace("{3}", String.valueOf(savedProduct));
    description = description.replace("{4}", String.valueOf(descriptions.get(2)));
    description = description.replace("{5}", String.valueOf(totalProduct - savedProduct));
    return description;
  }

  private String constructBulkDeleteProcessDescription(int savedProduct, int totalProduct) {
    String description = "";
    int failedProduct = totalProduct - savedProduct;
    String successMessage = multiPickupPointEnabled ? VARIANTS_SUCCESS_DELETED_IN : CLICK_AND_COLLECT_PRODUCTS_SUCCESS_DELETED_IN;
    String failedMessage = multiPickupPointEnabled ? VARIANTS_FAILED_DELETED_IN : CLICK_AND_COLLECT_PRODUCTS_FAILED_DELETED_IN;
    if (savedProduct == totalProduct) {
      description += String.format(successMessage, savedProduct);
    } else if (savedProduct > 0 && savedProduct < totalProduct) {
      description += String.format(successMessage, savedProduct);
      description += String.format(failedMessage, failedProduct);
      description += CHECK_FAILED_PRODUCTS_HERE_IN;
    } else if (savedProduct == 0) {
      description += String.format(failedMessage, failedProduct);
      description += CHECK_YOUR_EXCEL_FILE_IN;
    }
    return description;
  }

  /**
   * validate BLIBLI SKU format, will remove the row data that has invalid format
   *
   * @param rowDataFromExcel must not be null or empty
   */
  public static void validateRowDataFromExcel(List<Map<String, String>> rowDataFromExcel) {
    if (CollectionUtils.isNotEmpty(rowDataFromExcel)) {
      rowDataFromExcel.removeIf(rowData -> !validateItemSkuWhenUpload(
          Optional.ofNullable(rowData.get(BLIBLI_SKU_HEADER))
              .orElseGet(() -> rowData.get(BLIBLI_SKU_HEADER_WITH_EXAMPLE))));
    }
  }

  public static boolean validateItemSkuWhenUpload(String itemSku) {
    if (StringUtils.isNotBlank(itemSku)) {
      return ITEM_SKU_PATTERN.matcher(itemSku).matches();
    }
    return true;
  }

  public static boolean validateProductSkuWhenUpload(String productSku) {
    if (StringUtils.isNotBlank(productSku)) {
      return PRODUCT_SKU_PATTERN.matcher(productSku).matches();
    }
    return true;
  }

  public static boolean validateProductSkuAndSellerCodeWhenUpload(String productSku, String businessPartnerCode) {
    if (StringUtils.isNotBlank(productSku)) {
      return PRODUCT_SKU_PATTERN.matcher(productSku).matches() && productSku.startsWith(businessPartnerCode);
    }
    return true;
  }

  public static boolean validateVatUpdateDto(VatUpdateDto vatUpdateDto) {
    if (StringUtils.isNotBlank(vatUpdateDto.getItemCode()) && (BulkParameters.ENABLE_VALUE
        .equals(vatUpdateDto.getVatApplicable()) || BulkParameters.DISABLE_VALUE
        .equals(vatUpdateDto.getVatApplicable()))) {
      return true;
    }
    return false;
  }

  public static boolean validateItemSku(String itemSku) {
    return ITEM_SKU_PATTERN.matcher(itemSku).matches();
  }

  public Map<String, String> setBlpInitialData(BulkProcessData bulkProcessData, Map<String, Boolean> privilegedMap)
      throws IOException {
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    bulkProcessData.setStartDate(new Date());
    LinkedHashMap<String, String> rowDataJson = objectMapper.readValue(bulkProcessData.getBulkRequestData(),
        new TypeReference<LinkedHashMap<String, String>>() {
        });
    privilegedMap.putAll(objectMapper.readValue(rowDataJson.get(Constant.PRIVILEGED_MAP),
        new TypeReference<LinkedHashMap<String, Boolean>>() {
        }));
    rowDataJson.remove(Constant.PRIVILEGED_MAP);
    return rowDataJson;
  }

  public void setDataFinalStatus(List<BulkProcessData> bulkProcessDataList,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList)
      throws JsonProcessingException {
    Map<String, BulkUpdateErrorDTO> itemSkuErrorMap = listBulkUpdateErrorDTO.stream()
        .collect(Collectors.toMap(BulkUpdateErrorDTO::getProductSku, Function.identity()));
    Map<String, BulkUpdateSuccessDTO> itemSkuSuccessMap = bulkUpdateSuccessDTOList.stream()
        .collect(Collectors.toMap(BulkUpdateSuccessDTO::getProductSku, Function.identity()));
    for (BulkProcessData data : bulkProcessDataList) {
      if (itemSkuErrorMap.containsKey(data.getParentProduct())) {
        data.setErrorMessage(objectMapper.writeValueAsString(itemSkuErrorMap.get(data.getParentProduct())));
        data.setStatus(BulkProcessData.STATUS_FAIL);
        if (itemSkuErrorMap.get(data.getParentProduct()).getReason().contains(INTERNAL_ERROR)) {
          data.setSystemErrorCount(1);
        } else {
          data.setInputErrorCount(1);
        }
      } else {
        data.setNotes(objectMapper.writeValueAsString(itemSkuSuccessMap.get(data.getParentProduct())));
        data.setStatus(BulkProcessData.STATUS_SUCCESS);
      }
      data.setEndDate(new Date());
    }
  }

  public void setDataFinalStatusL5(List<BulkProcessData> bulkProcessDataList,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList)
      throws IOException {
    Map<String, BulkUpdateErrorDTO> itemSkuErrorMap = listBulkUpdateErrorDTO.stream()
        .collect(Collectors.toMap(BulkUpdateErrorDTO::getPickupPointCode, Function.identity()));
    Map<String, BulkUpdateSuccessDTO> itemSkuSuccessMap = bulkUpdateSuccessDTOList.stream()
        .collect(Collectors.toMap(BulkUpdateSuccessDTO::getPickupPointCode, Function.identity()));
    for (BulkProcessData data : bulkProcessDataList) {
      LinkedHashMap<String, String> rowDataJson =
          objectMapper.readValue(data.getBulkRequestData(), new TypeReference<LinkedHashMap<String, String>>() {
          });
      if (itemSkuErrorMap.containsKey(rowDataJson.get(BulkParameters.PICKUP_POINT_HEADER))) {
        data.setErrorMessage(
            objectMapper.writeValueAsString(itemSkuErrorMap.get(rowDataJson.get(BulkParameters.PICKUP_POINT_HEADER))));
        data.setStatus(BulkProcessData.STATUS_FAIL);
        if (itemSkuErrorMap.get(rowDataJson.get(BulkParameters.PICKUP_POINT_HEADER)).getReason()
            .contains(INTERNAL_ERROR)) {
          data.setSystemErrorCount(1);
        } else {
          data.setInputErrorCount(1);
        }
      } else {
        data.setNotes(objectMapper.writeValueAsString(
            itemSkuSuccessMap.get(rowDataJson.get(BulkParameters.PICKUP_POINT_HEADER))));
        data.setStatus(BulkProcessData.STATUS_SUCCESS);
      }
      data.setEndDate(new Date());
    }
  }

  public void setEanUpdateFinalDataStatus(List<BulkProcessData> bulkProcessDataList,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList)
      throws IOException {
    Map<String, BulkUpdateErrorDTO> itemSkuErrorMap = listBulkUpdateErrorDTO.stream()
        .collect(Collectors.toMap(BulkUpdateErrorDTO::getProductSku, Function.identity()));
    Map<String, BulkUpdateSuccessDTO> itemSkuSuccessMap = bulkUpdateSuccessDTOList.stream()
        .collect(Collectors.toMap(BulkUpdateSuccessDTO::getProductSku, Function.identity()));
    for (BulkProcessData data : bulkProcessDataList) {
      LinkedHashMap<String, String> rowDataJson;
      rowDataJson = getBulkRequest(data);
      if (itemSkuErrorMap.containsKey(rowDataJson.get(BulkParameters.BLIBLI_SKU))) {
        data.setErrorMessage(
            objectMapper.writeValueAsString(itemSkuErrorMap.get(rowDataJson.get(BulkParameters.BLIBLI_SKU))));
        data.setStatus(BulkProcessData.STATUS_FAIL);
        if (itemSkuErrorMap.get(rowDataJson.get(BulkParameters.BLIBLI_SKU)).getReason()
            .contains(INTERNAL_ERROR)) {
          data.setSystemErrorCount(1);
        } else {
          data.setInputErrorCount(1);
        }
      } else {
        data.setNotes(objectMapper.writeValueAsString(
            itemSkuSuccessMap.get(rowDataJson.get(BulkParameters.BLIBLI_SKU))));
        data.setStatus(BulkProcessData.STATUS_SUCCESS);
      }
      data.setEndDate(new Date());
    }
  }

  private LinkedHashMap<String, String> getBulkRequest(BulkProcessData data) {
    LinkedHashMap<String, String> rowDataJson;
    try {
      rowDataJson =
          objectMapper.readValue(data.getBulkRequestData(), new TypeReference<LinkedHashMap<String, String>>() {
          });
    } catch (JsonProcessingException exception) {
      log.error("Error while parsing json {}", data.getBulkRequestData(), exception);
      rowDataJson = new LinkedHashMap<>();
    }
    return rowDataJson;
  }

  public void setFinalStatusForSystemFailure(List<BulkProcessData> bulkProcessDataList, BulkProcess bulkProcess)
      throws Exception {
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
      bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessNotes.setBulkProcess(bulkProcess);
      bulkProcessNotes.setNotes(
          Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD + BulkUpdateServiceUtil.INTERNAL_ERROR);
      bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
      bulkProcessData.setSystemErrorCount(1);
      bulkProcessData.setEndDate(new Date());
      BulkUpdateErrorDTO bulkUpdateErrorDTO = BulkUpdateErrorDTO.builder().reason(bulkProcessNotes.getNotes())
          .productSku(bulkProcessData.getParentProduct()).build();
      bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkUpdateErrorDTO));
    }
  }

  public AuditTrailInfo getAuditTrailInfo(BulkProcess bulkProcess) {
    return AuditTrailInfo.builder().businessPartnerCode(bulkProcess.getBusinessPartnerCode())
        .requestId(bulkProcess.getRequestId()).remoteAddress(Constant.DEFAULT_CLIENT_HOST)
        .username(bulkProcess.getCreatedBy()).build();
  }

  public WholeSaleCount updateBulkFinalStatusAndSendNotification(BulkProcess bulkProcess, int totalProduct,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList,
      boolean isInternationalMerchant) {
    int successCount = bulkUpdateSuccessDTOList.size();
    long sameThresholdCount = bulkUpdateSuccessDTOList.stream()
        .filter(bulkUpdateSuccessDTO -> Objects.nonNull(bulkUpdateSuccessDTO.getSameThreshold()))
        .filter(BulkUpdateSuccessDTO::getSameThreshold).count();
    long differentThresholdCount = bulkUpdateSuccessDTOList.stream()
        .filter(bulkUpdateSuccessDTO -> Objects.nonNull(bulkUpdateSuccessDTO.getSameThreshold()))
        .filter(bulkUpdateSuccessDTO -> !bulkUpdateSuccessDTO.getSameThreshold()).count();
    WholeSaleCount wholeSaleCount = null;
    if (listBulkUpdateErrorDTO.isEmpty()) {
      if(isInternationalMerchant) {
        bulkProcess.setDescription(ALL_PRODUCTS_UPDATED_EN);
      } else {
        bulkProcess.setDescription(ALL_PRODUCTS_UPDATED_IN);
      }
    } else if (successCount == 0) {
      if (isInternationalMerchant) {
        bulkProcess.setDescription(ALL_PRODUCTS_FAILED_EN);
      } else {
        bulkProcess.setDescription(ALL_PRODUCTS_FAILED_IN);
      }
      return null;
    } else if (successCount < totalProduct && sameThresholdCount == 0
        && differentThresholdCount == 0) {
      if (isInternationalMerchant) {
        bulkProcess.setDescription(String.format(PARTIAL_UPDATE_EN, successCount, totalProduct));
      } else {
        bulkProcess.setDescription(String.format(PARTIAL_UPDATE_IN, successCount, totalProduct));
      }
    }
    if (sameThresholdCount != 0 || differentThresholdCount != 0) {
      if (sameThresholdCount == 0) {
        if (isInternationalMerchant) {
          bulkProcess.setDescription(String.format(DIFFERENT_THRESHOLD,
              successCount, differentThresholdCount, listBulkUpdateErrorDTO.size()));
        } else {
          bulkProcess.setDescription(String.format(DIFFERENT_THRESHOLD_IN,
              successCount, differentThresholdCount, listBulkUpdateErrorDTO.size()));
        }
      } else if (differentThresholdCount == 0) {
        if (isInternationalMerchant) {
          bulkProcess.setDescription(String.format(SAME_THRESHOLD,
              successCount, sameThresholdCount, listBulkUpdateErrorDTO.size()));
        } else {
          bulkProcess.setDescription(String.format(SAME_THRESHOLD_IN,
              successCount, sameThresholdCount, listBulkUpdateErrorDTO.size()));
        }
      } else {
        if (isInternationalMerchant) {
          bulkProcess.setDescription(String.format(BOTH_SAME_AND_DIFFERENT_THRESHOLD, successCount,
              differentThresholdCount, sameThresholdCount, listBulkUpdateErrorDTO.size()));
        } else {
          bulkProcess.setDescription(String.format(BOTH_SAME_AND_DIFFERENT_THRESHOLD_IN, successCount,
              differentThresholdCount, sameThresholdCount, listBulkUpdateErrorDTO.size()));
        }
      }
      wholeSaleCount = WholeSaleCount.builder().wholeSaleTurnOffCount(differentThresholdCount)
          .wholeSaleUpdatedCount(sameThresholdCount).wholeSaleFailedCount(listBulkUpdateErrorDTO.size()).build();
    }
    return wholeSaleCount;
  }

  public void getFailedSuccessDto(List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList, BulkProcess bulkProcess,
      List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList, List<BulkProcessData> rowDataList) throws IOException {
    List<BulkProcessData> successRowData =
        rowDataList.stream().filter(rowData -> BulkProcessData.STATUS_SUCCESS.equals(rowData.getStatus()))
            .collect(Collectors.toList());
    List<BulkProcessData> failRowData =
        rowDataList.stream().filter(rowData -> BulkProcessData.STATUS_FAIL.equals(rowData.getStatus()))
            .collect(Collectors.toList());
    for (BulkProcessData bulkProcessData : successRowData) {
      TypeReference<BulkUpdateSuccessDTO> typ = new TypeReference<BulkUpdateSuccessDTO>() {
      };
      BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
          getObjectMapper().readValue(String.valueOf(bulkProcessData.getNotes()), typ);
      bulkUpdateSuccessDTOList.add(bulkUpdateSuccessDTO);
    }
    int inputError = 0;
    int systemError = 0;
    for (BulkProcessData bulkProcessData : failRowData) {
      TypeReference<BulkUpdateErrorDTO> typ = new TypeReference<BulkUpdateErrorDTO>() {
      };
      BulkUpdateErrorDTO bulkUpdateErrorDTO = null;
      if (Constant.SYSTEM_ERROR.equals(bulkProcessData.getErrorMessage())) {
        bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
        bulkUpdateErrorDTO.setReason(bulkProcessData.getErrorMessage());
      } else {
        bulkUpdateErrorDTO = getObjectMapper().readValue(String.valueOf(bulkProcessData.getErrorMessage()), typ);
      }
      bulkUpdateErrorDTOList.add(bulkUpdateErrorDTO);
      if (Optional.ofNullable(bulkProcessData.getInputErrorCount()).orElse(Constant.ZERO) == 1) {
        inputError++;
      } else {
        systemError++;
      }
    }
    bulkProcess.setInputErrorCount(inputError);
    bulkProcess.setSystemErrorCount(systemError);
    bulkProcess.setSuccessCount(successRowData.size());
    bulkProcess.setErrorCount(rowDataList.size() - successRowData.size());
    bulkProcess.setEndDate(Calendar.getInstance().getTime());
    if (successRowData.size() == rowDataList.size()) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      if (CollectionUtils.isNotEmpty(successRowData)) {
        bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
      } else {
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      }
    }
  }

  public static BulkCncUpsertErrorDTO validateDataForPickupUpsert(Map<String, String> rowData,
      boolean isInternationalMerchant) {
    StringBuilder tempErrorMessage =
        new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(IS).append(rowData.get(BulkParameters.BLIBLI_SKU))
            .append(END_SYMBOL).append(FAILURE_REASON).append(IS);
    boolean successFlag = checkBlibliSKU(rowData, tempErrorMessage);
    successFlag = checkFieldValueMustBeNumberAndGreaterThanZero(rowData, successFlag, tempErrorMessage,
        isInternationalMerchant ? ExcelHeaderNames.LIST_PRICE_EN : ExcelHeaderNames.LIST_PRICE_IN);
    successFlag = checkFieldValueMustBeNumberAndGreaterThanZero(rowData, successFlag, tempErrorMessage,
        isInternationalMerchant ? ExcelHeaderNames.OFFER_PRICE_EN : ExcelHeaderNames.OFFER_PRICE_IN);
    successFlag = checkFieldValueMustBeNumberAndPositive(rowData, successFlag, tempErrorMessage,
        isInternationalMerchant ? ExcelHeaderNames.STOCK_EN : ExcelHeaderNames.STOCK_IN);
    successFlag = checkPickupPointNotEmpty(rowData, successFlag, BulkParameters.PICKUP_POINT_CODE,
      PICKUP_POINT_EMPTY, tempErrorMessage);
    if (!successFlag) {
      String pickupPointCodeParam =
          isInternationalMerchant ? ExcelHeaderNames.PICKUP_POINT_CODE_EN : ExcelHeaderNames.PICKUP_POINT_CODE_IN;
      String errorMessage = removeLastAmpSymbol(tempErrorMessage.toString());
      return new BulkCncUpsertErrorDTO(rowData.get(BulkParameters.BLIBLI_SKU), rowData.get(pickupPointCodeParam),
          errorMessage);
    }
    return null;
  }

  public List<Map<String, String>> getRowDataToProcessSuspension(List<BulkInternalProcessData> internalProcessData)
      throws IOException {
    List<Map<String, String>> userInputRows = new ArrayList<>();
    for (BulkInternalProcessData bulkProcessData : internalProcessData) {
      LinkedHashMap<String, String> rowDataJson =
          new ObjectMapper().readValue(bulkProcessData.getData(), new TypeReference<LinkedHashMap<String, String>>() {
          });
      userInputRows.add(rowDataJson);
    }
    return userInputRows;
  }

  public void setBlpFinalDataForSuspension(List<BulkInternalProcessData> internalProcessData,
      List<Map<String, String>> failedMapList) throws JsonProcessingException {
    setFinalDataForInternalProcess(internalProcessData, failedMapList, BulkProductSuspensionParameters.PRODUCT_CODE);
  }

  public void setBlpFinalDataForConfigUpdate(List<BulkInternalProcessData> internalProcessData,
      List<Map<String, String>> failedMapList) throws JsonProcessingException {
    String param = internalProcessData.get(0).getProcessType();
    if (param.equalsIgnoreCase(BulkConfigurationUpdateParameters.MERCHANT)) {
      param = BulkConfigurationUpdateParameters.SELLER_CODE;
    } else {
      param = BulkConfigurationUpdateParameters.CATEGORY_CODE;
    }
    setFinalDataForInternalProcess(internalProcessData, failedMapList, param);
  }

  private void setFinalDataForInternalProcess(List<BulkInternalProcessData> internalProcessData,
      List<Map<String, String>> failedMapList, String param) throws JsonProcessingException {
    Map<String, Map<String, String>> productCodeErrorMsgMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(failedMapList)) {
      productCodeErrorMsgMap.putAll(
          failedMapList.stream().collect(Collectors.toMap(failedData -> failedData.get(param), Function.identity())));
    }
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessData) {
      if (productCodeErrorMsgMap.containsKey(bulkInternalProcessData.getParentCode())) {
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
        bulkInternalProcessData.setErrorMessage(objectMapper.writeValueAsString(
          productCodeErrorMsgMap.get(bulkInternalProcessData.getParentCode())
            .getOrDefault(SUSPENSION_FAILED_REASON,StringUtils.EMPTY)));
      } else {
        bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
      }
    }
  }

  public static void addToMap(String key, BulkProcessData bulkProcessData, Map<String, List<BulkProcessData>> dataMap) {
    if (dataMap.containsKey(key)) {
      List<BulkProcessData> bulkProcessDataList = dataMap.get(key);
      bulkProcessDataList.add(bulkProcessData);
      dataMap.put(key, bulkProcessDataList);
    } else {
      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      bulkProcessDataList.add(bulkProcessData);
      dataMap.put(key, bulkProcessDataList);
    }
  }

  public int getFinalDataList(Map<String, List<BulkProcessData>> dataMap, List<BulkProcessData> dataList,
      boolean isBulkUpdate, boolean isBulkUpsert)
      throws Exception {
    int errorCount = 0;
    for (Map.Entry<String, List<BulkProcessData>> entry : dataMap.entrySet()) {
      List<BulkProcessData> processDataList = entry.getValue();
      if (processDataList.size() > 1) {
        for (BulkProcessData bulkProcessData : processDataList) {
          Map<String, String> rowData = getObjectMapper().readValue(bulkProcessData.getBulkRequestData(),
              new TypeReference<LinkedHashMap<String, String>>() {
              });
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          bulkProcessData.setInputErrorCount(1);
          if (isBulkUpdate) {
            if(multiPickupPointEnabled){
              bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
                new BulkUpdateErrorDTO(
                  rowData.get(BulkParameters.BLIBLI_SKU) + Constant.DASH + rowData
                    .get(BulkParameters.BLIBLI_SKU),
                  BulkUpdateServiceUtil.DUPLICATE_OFFLINE_ITEM)));
            }
            else {
              bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
                new BulkUpdateErrorDTO(rowData.get(BulkParameters.BLIBLI_SKU), BulkUpdateServiceUtil.DUPLICATE_BLIBLI_SKU)));
            }
          } else if (isBulkUpsert) {
            bulkProcessData.setErrorMessage(getObjectMapper().writeValueAsString(
                new BulkCncUpsertErrorDTO(rowData.get(ExcelHeaderNames.BLIBLI_SKU_EN),
                    rowData.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN), BulkUpdateServiceUtil.DUPLICATE_OFFLINE_ITEM)));
          }
          else {
            bulkProcessData.setErrorMessage(getObjectMapper().writeValueAsString(
                new BulkCncUpsertErrorDTO(rowData.get(BulkParameters.BLIBLI_SKU),
                    rowData.get(BulkParameters.PICKUP_POINT_CODE), BulkUpdateServiceUtil.DUPLICATE_OFFLINE_ITEM)));
          }
          errorCount++;
        }
      } else {
        if (BulkProcessData.STATUS_FAIL.equalsIgnoreCase(processDataList.get(0).getStatus())) {
          errorCount++;
        }
      }
      dataList.addAll(processDataList);
    }
    return errorCount;
  }

  public int validateDuplicateFbbPickupPoints(String businessPartnerCode, List<BulkProcessData> dataList,
      boolean isUpsert, ProfileResponse profileResponse) throws Exception {
    int errorCount = 0;
    Map<String, List<Pair<String, BulkProcessData>>> uploadedRowDataMap = new HashMap<>();
    Map<String, List<Pair<String, BulkProcessData>>> duplicateUploadedRowDataMap = new HashMap<>();

    if (CollectionUtils.isNotEmpty(dataList)) {
      for (BulkProcessData bulkProcessData : dataList) {
        Map<String, String> rowData = objectMapper.readValue(bulkProcessData.getBulkRequestData(),
            new TypeReference<LinkedHashMap<String, String>>() {
            });
        String itemSku =
            isUpsert ? rowData.get(ExcelHeaderNames.BLIBLI_SKU_EN) : rowData.get(BulkParameters.BLIBLI_SKU);
        String pickupPointCode = isUpsert ?
            rowData.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN) :
            rowData.get(BulkParameters.PICKUP_POINT_HEADER);
        if (uploadedRowDataMap.containsKey(itemSku)) {
          uploadedRowDataMap.get(itemSku).add(Pair.of(pickupPointCode, bulkProcessData));
        } else {
          uploadedRowDataMap.put(itemSku, new ArrayList<>(Arrays.asList(Pair.of(pickupPointCode, bulkProcessData))));
        }
      }

      for (Map.Entry<String, List<Pair<String, BulkProcessData>>> entry : uploadedRowDataMap.entrySet()) {
        if (entry.getValue().size() > 1) {
          duplicateUploadedRowDataMap.put(entry.getKey(), entry.getValue());
        }
      }

      if (MapUtils.isNotEmpty(duplicateUploadedRowDataMap)) {
        Set<String> fbbPickupPoint = getAllFbbPickupPointsForSeller(businessPartnerCode,
            duplicateUploadedRowDataMap.values().stream().flatMap(List::stream).map(Pair::getLeft)
                .collect(Collectors.toSet()));
        if (Objects.isNull(profileResponse)) {
          profileResponse = businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
              businessPartnerCode);
        }
        for (Map.Entry<String, List<Pair<String, BulkProcessData>>> entry : duplicateUploadedRowDataMap.entrySet()) {
          int count = 0;
          for (Pair<String, BulkProcessData> ppCodeAndDataPair : entry.getValue()) {
            if (fbbPickupPoint.contains(ppCodeAndDataPair.getLeft())) {
              if (count > 0 && (!mppForWhEnabled || !ConverterUtil.checkIfMPPIsAllowed(
                  mppAllowedSellers, profileResponse))) {
                ppCodeAndDataPair.getRight().setStatus(BulkProcessData.STATUS_FAIL);
                ppCodeAndDataPair.getRight().setErrorMessage(objectMapper.writeValueAsString(
                    new BulkCncUpsertErrorDTO(entry.getKey(), ppCodeAndDataPair.getLeft(),
                        BulkUpdateServiceUtil.DUPLICATE_FBB_PICKUP_POINT)));
                errorCount++;
              }
              count++;
            }
          }
        }
      }
    }
    return errorCount;
  }

  private Set<String> getAllFbbPickupPointsForSeller(String businessPartnerCode, Set<String> pickupPointCodes) throws ApplicationException {
    int page = 0;
    long totalElements = 0L;
    List<PickupPointResponse> pickupPointResponses = new ArrayList<>();
    do {
      Page<PickupPointResponse> response =
          businessPartnerRepository.filterBusinessPartnerPickupPointV2(page, fbbPickupPointFetchSize,
              PickupPointFilterRequest.builder().codes(pickupPointCodes).businessPartnerCode(businessPartnerCode)
                  .fbbActivated(true)
                  .waitingDeletion(RequestHelper.getWaitingDeletion(setWaitingDeletionForDeletePickupPoint)).build());
      pickupPointResponses.addAll(response.getContent());
      page++;
      totalElements = response.getTotalElements();
    } while (page * fbbPickupPointFetchSize < totalElements);
    return pickupPointResponses.stream().map(PickupPointResponse::getCode).collect(Collectors.toSet());
  }

  /**
   * Get Product Detail Data from Xprod
   *
   * @param cleanDatas
   * @return Map of ItemSku + "-" + item pp code -> ItemSummaryListResponse
   */
  private Map<String, ItemSummaryListResponse> getItemSummaryData(
    List<Map<String, String>> cleanDatas) {
    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest>
      itemPickupPointRequestList = BulkCreationCommonUtil.toItemSkuAndPPCodeRequest(cleanDatas);

    Map<String, ItemSummaryListResponse> itemSummaryData = new HashMap<>();
    List<ItemSummaryListResponse> itemSummaryListResponses =
      xProductOutboundService.getItemSummaryByItemSkuAndPPCode(itemPickupPointRequestList);

    itemSummaryListResponses.forEach(itemSummary -> itemSummaryData.putIfAbsent(
      itemSummary.getItemSku() + Constant.DASH + itemSummary
        .getPickupPointCode(), itemSummary));

    return itemSummaryData;
  }

  /**
   * Get Campaign V2 Product Details from X-Campaign
   *
   * @param cleanDatas
   * @param bulkAddCampaignProductQueue
   * @return Map of ItemSku + "-" + item pp code -> CampaignProductDetailResponse
   * @throws Exception
   */
  private Map<String, CampaignProductDetailResponse> getCampaignProductDetailsV2(
    List<Map<String, String>> cleanDatas, BulkAddCampaignProductQueue bulkAddCampaignProductQueue)
    throws Exception {
    Map<String, CampaignProductDetailResponse> campaignProductMap = new HashMap<>();
    List<ItemDetailsDto> itemDetailsDtos = BulkCreationCommonUtil.toItemListDtoList(cleanDatas);

    List<CampaignProductDetailResponse> campaignProductDetailResponseList =
      this.getCampaignRepository()
        .getCampaignProductDetailsV2(itemDetailsDtos, bulkAddCampaignProductQueue);

    campaignProductDetailResponseList.forEach(campaignDetail -> campaignProductMap.putIfAbsent(
      campaignDetail.getItemSku() + Constant.DASH + campaignDetail.getPickUpPointCode(),
      campaignDetail));

    return campaignProductMap;
  }

  /**
   * validate quota and prices when both campaign and product response exist V2
   * @param bulkUpdateServiceValidation
   * @param itemSkuIndex
   * @param itemSkuPPCode
   * @return true or false
   */
  private boolean isQuotaAndPriceValidatedV2(BulkUpdateServiceValidation bulkUpdateServiceValidation,
    int itemSkuIndex, String itemSkuPPCode) {

    return this.isInputQuotaGreaterThanUsedQuota(
      bulkUpdateServiceValidation.getTempErrorMessage(),
      bulkUpdateServiceValidation.getTempCleanDataList().get(itemSkuIndex),
      bulkUpdateServiceValidation.getCampaignProductDetailResponseMap().get(itemSkuPPCode))
      && this.isProductIncludedInCampaign(bulkUpdateServiceValidation.getBulkAddCampaignProductQueue(),
      bulkUpdateServiceValidation.getTempErrorMessage(), BulkCreationCommonUtil
        .toProductLevel3SummaryResponse(
          bulkUpdateServiceValidation.getItemSummaryMap().get(itemSkuPPCode)));
  }

  /**
   * validate prices from product response when campaign response does not exist
   * @param bulkUpdateServiceValidation
   * @param itemSkuIndex
   * @param itemSkuPPCode
   * @return true or false
   */
  private boolean isPriceValidatedWhenCampaignEqualsNullV2(BulkUpdateServiceValidation bulkUpdateServiceValidation,
    int itemSkuIndex, String itemSkuPPCode) {

    return this.isProductIncludedInCampaign(
      bulkUpdateServiceValidation.getBulkAddCampaignProductQueue(),
      bulkUpdateServiceValidation.getTempErrorMessage(),
      BulkCreationCommonUtil.toProductLevel3SummaryResponse(bulkUpdateServiceValidation.getItemSummaryMap().get(itemSkuPPCode)))
      && this.isPriceCutGreaterThanSellingPrice(
      bulkUpdateServiceValidation.getTempErrorMessage(),
      bulkUpdateServiceValidation.getTempCleanDataList().get(itemSkuIndex), BulkCreationCommonUtil
        .toProductLevel3SummaryResponse(
          bulkUpdateServiceValidation.getItemSummaryMap().get(itemSkuPPCode)));
  }

  public static void validateAndSetStock(Map<String, String> data,
    UpsertOfflineItemFailedResponse validationFailedResponse, UpsertOfflineItemRequest request,
    UpsertValidationDTO upsertValidationDto) {
    int stock = 0;
    if (Strings.isNotEmpty(data.get(ExcelHeaderNames.STOCK_EN))) {
      try {
        stock = (int) Double.parseDouble(data.get(ExcelHeaderNames.STOCK_EN));
        request.setStock(stock);
      } catch (Exception ex) {
        validationFailedResponse.setErrorCode(
          BulkProcessValidationErrorMessages.STOCK_MUST_BE_NUMBER);
        upsertValidationDto.setResult(false);
      }
      if (stock < 0) {
        validationFailedResponse.setErrorCode(
          BulkProcessValidationErrorMessages.STOCK_MUST_BE_POSITIVE_VALUE);
        upsertValidationDto.setResult(false);
      }
    } else {
      validationFailedResponse.setErrorCode(
        BulkProcessValidationErrorMessages.STOCK_MUST_NOT_BE_BLANK);
      upsertValidationDto.setResult(false);
    }
  }

  public static void validateAndSetCncStatus(Map<String, String> data,
    UpsertOfflineItemFailedResponse validationFailedResponse, UpsertOfflineItemRequest request,
    UpsertValidationDTO upsertValidationDto) {
    String cncActive = data.get(ExcelHeaderNames.CNC_STATUS_EN);
    if (StringUtils.isEmpty(cncActive) || (BulkParameters.POSSIBLE_OFFLINE_VALUES).contains(
      cncActive)) {
      request.setCncActive(false);
    } else if ((BulkParameters.POSSIBLE_ONLINE_VALUES).contains(cncActive)) {
      request.setCncActive(true);
    } else {
      validationFailedResponse.setErrorCode(
        GenericErrorMessages.INVALID_CNC_STATUS_VALUE_ON_CNC_STATUS_FIELD);
      upsertValidationDto.setResult(false);
    }
  }

  public static void validateAndSetOnlineStatus(Map<String, String> data,
    UpsertOfflineItemFailedResponse validationFailedResponse, UpsertOfflineItemRequest request,
      UpsertValidationDTO upsertValidationDto, boolean cncForWarehouseFeatureSwitch) {
    String online = cncForWarehouseFeatureSwitch ?
        data.get(ExcelHeaderNames.DELIVERY_STATUS_EN_CNC_1P) :
        data.get(ExcelHeaderNames.DELIVERY_STATUS_EN);
    if ((BulkParameters.POSSIBLE_ONLINE_VALUES).contains(online)) {
      request.setBuyable(true);
      request.setDiscoverable(true);
    } else if (StringUtils.isEmpty(online) || (BulkParameters.POSSIBLE_OFFLINE_VALUES).contains(
      online)) {
      request.setBuyable(false);
      request.setDiscoverable(false);
    } else {
      validationFailedResponse.setErrorCode(
        GenericErrorMessages.INVALID_STATUS_VALUE_ON_STATUS_FIELD);
      upsertValidationDto.setResult(false);
    }
  }

  public static String constructOfflineItemId(UpsertOfflineItemRequest offlineItemRequest) {
    return offlineItemRequest.getItemSku().concat(Constant.HYPHEN)
      .concat(offlineItemRequest.getPickupPointCode());
  }

  public void setScheduleRemovalForBulkProcessUpdateAndUpsert(
    List<BulkProcessData> bulkProcessDataList, Boolean scheduleRemoval,
    boolean internationalMerchant, String productSku, String productName) {
    if (Boolean.TRUE.equals(scheduleRemoval) && updateScheduleRemovalEnabled) {
      String removalMessage = internationalMerchant ?
        BulkUpdateServiceUtil.SCHEDULE_REMOVAL_IN_MESSAGE :
        BulkUpdateServiceUtil.SCHEDULE_REMOVAL_MESSAGE;

      String formattedMessage =
        String.format(removalMessage, productName, sellerPDPUrl.concat(productSku));
      bulkProcessDataList.stream().filter(
          bulkProcessData -> StringUtils.isNotBlank(bulkProcessData.getParentProduct())
            && bulkProcessData.getParentProduct()
            .substring(0, bulkProcessData.getParentProduct().lastIndexOf(Constant.DASH))
            .equals(productSku))
        .forEach(bulkProcessData -> bulkProcessData.setIdentifier(formattedMessage));
    }
  }

  public static String extractProductSku(String productSku) {
    int firstDashIndex = productSku.indexOf("-");
    int secondDashIndex = productSku.indexOf("-", firstDashIndex + 1);
    int thirdDashIndex = productSku.indexOf("-", secondDashIndex + 1);
    // for  upsert itemSku is stored in parent product column while it is productSku for update
    return (thirdDashIndex != -1) ? productSku.substring(0, thirdDashIndex) : productSku;
  }

  public static Set<String> getSellerTypes(String bopisCategoryValidationForSellerTypes) {
    return Optional.ofNullable(bopisCategoryValidationForSellerTypes)
        .map(value -> Arrays.stream(value.split(Constant.COMMA)).map(String::trim).collect(Collectors.toSet()))
        .orElse(Collections.emptySet());
  }

  public static boolean isBopisProductMadeCnc(boolean bopisCncRestrictionEnabled, boolean cncActive,
      Integer productType) {
    return bopisCncRestrictionEnabled && cncActive && (Objects.nonNull(productType)
        && ProductType.BOPIS.getCode() == productType);
  }

  public static boolean isDimensionMissing(boolean bopisCategoryRestrictionEnabled, boolean buyable,
      ProfileResponse profileResponse, Boolean dimensionMissing, Set<String> sellerTypes) {
    return bopisCategoryRestrictionEnabled && sellerTypes.contains(profileResponse.getCompany().getMerchantType())
        && Boolean.TRUE.equals(dimensionMissing) && buyable;
  }

  public static boolean validateItemSkuByBusinessPartnerCode(String itemSku, String businessPartnerCode) {
      return ITEM_SKU_PATTERN.matcher(itemSku).matches() && itemSku.startsWith(businessPartnerCode);
  }

  public static boolean isMerchantEligibleForValidation(ProfileResponse profile,
    String supportedMerchantsForWarehouseStockValidation) {
    boolean isEligible = false;
    if (Objects.nonNull(profile)) {
      String merchantType =
        Optional.ofNullable(profile.getCompany()).map(CompanyDTO::getMerchantType)
          .orElse(StringUtils.EMPTY);
      isEligible =
        getSupportedMerchants(supportedMerchantsForWarehouseStockValidation).contains(merchantType)
          && profile.isFbbActivated();
    }
    return isEligible;
  }

  public static Set<String> getSupportedMerchants(
    String supportedMerchantsForWarehouseStockValidation) {
    return Optional.ofNullable(supportedMerchantsForWarehouseStockValidation).map(
      value -> Arrays.stream(value.split(Constant.COMMA)).map(String::trim)
        .collect(Collectors.toSet())).orElse(Collections.emptySet());
  }

  public static InventoryDetailInfoRequestDTO toInventoryDetailInfoRequestDTO(
    DeleteOfflineItemRequest deleteOfflineItemRequest) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
      new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setPickupPointCode(deleteOfflineItemRequest.getPickupPointCode());
    inventoryDetailInfoRequestDTO.setWebItemSku(deleteOfflineItemRequest.getItemSku());
    inventoryDetailInfoRequestDTO.setWebMerchantCode(deleteOfflineItemRequest.getMerchantSku());
    return inventoryDetailInfoRequestDTO;
  }

  public static void validateWareHouseStockAvailabilityForPPDeletion(
    InventoryDetailInfoResponseDTO inventoryResponse,
    DeleteOfflineItemRequest deleteOfflineItemRequest,
    List<DeleteOfflineItemDetailResponse> responses) {
    int warehouseAvailable = getWarehouseAvailableStockValue(inventoryResponse);
    if (warehouseAvailable > 0) {
      // distribution + non distribution
      DeleteOfflineItemDetailResponse deleteResponse = new DeleteOfflineItemDetailResponse();
      deleteResponse.setErrorMessage(BulkProcessValidationErrorMessages.L5_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK);
      deleteResponse.setItemSku(deleteOfflineItemRequest.getItemSku());
      deleteResponse.setPickupPointCode(deleteOfflineItemRequest.getPickupPointCode());
      log.info("Found Warehouse stock linked to L5 : {} intended for Deletion ",
        deleteOfflineItemRequest.getItemSku().concat(Constant.HYPHEN).concat(deleteOfflineItemRequest.getPickupPointCode()));
      responses.add(deleteResponse);
    }
  }

  private static int getWarehouseAvailableStockValue(
    InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO) {
    int warehouseAvailable = 0;
    // get stock available in distribution WH
    warehouseAvailable += Optional.ofNullable(inventoryDetailInfoResponseDTO)
      .map(InventoryDetailInfoResponseDTO::getWarehouseInventoryResponseList)
      .orElse(Collections.emptyList()).stream()
      .mapToInt(WarehouseInventoryResponseDTO::getAvailableStock).sum();
    //  stock available in non distribution WH added with stock in distribution
    warehouseAvailable += Optional.ofNullable(inventoryDetailInfoResponseDTO)
      .map(InventoryDetailInfoResponseDTO::getNonDistributionWarehouseInventoryResponseList)
      .orElse(Collections.emptyList()).stream()
      .mapToInt(WarehouseInventoryResponseDTO::getAvailableStock).sum();
    return warehouseAvailable;
  }

  public static void bulkProcessBasicInfoValidation(BulkBasicInfoRequest bulkBasicInfoRequest) {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(bulkBasicInfoRequest.getBulkProcessType()),
        GenericErrorMessages.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(bulkBasicInfoRequest.getFileName()),
        GenericErrorMessages.FILE_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(bulkBasicInfoRequest.getBusinessPartnerCode()),
        GenericErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
  }

  public static void setFinalStatusForInputFailure(BulkProcessData bulkProcessData,
      BulkProcess bulkProcess, String errorMessage, int inputErrorCount, int systemErrorCount) {
    bulkProcessData.setInputErrorCount(inputErrorCount);
    bulkProcessData.setSystemErrorCount(systemErrorCount);
    BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
    bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessNotes.setBulkProcess(bulkProcess);
    bulkProcessNotes.setNotes(
        Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD + errorMessage);
    bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData.setEndDate(new Date());
    bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
  }

  public CellStyle createErrorHeaderStyle(Workbook workbook) {
    CellStyle style = workbook.createCellStyle();
    style.setFillForegroundColor(IndexedColors.RED.getIndex());
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    Font font = workbook.createFont();
    font.setBold(true);
    font.setColor(IndexedColors.BLACK.getIndex());
    style.setFont(font);
    style.setAlignment(HorizontalAlignment.CENTER);
    style.setVerticalAlignment(VerticalAlignment.CENTER);
    style.setWrapText(true);
    setBorders(style);
    return style;
  }

  public CellStyle createErrorMessageCellStyle(Workbook workbook) {
    CellStyle style = workbook.createCellStyle();
    style.setFillForegroundColor(IndexedColors.ROSE.getIndex());
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    style.setAlignment(HorizontalAlignment.CENTER);
    style.setVerticalAlignment(VerticalAlignment.CENTER);
    Font font = workbook.createFont();
    font.setColor(IndexedColors.BLACK.getIndex());
    style.setFont(font);
    style.setWrapText(true);
    setBorders(style);
    return style;
  }

  private void setBorders(CellStyle style) {
    style.setBorderTop(BorderStyle.THIN);
    style.setBorderBottom(BorderStyle.THIN);
    style.setBorderLeft(BorderStyle.THIN);
    style.setBorderRight(BorderStyle.THIN);
  }

  private static boolean validateMandatoryColumn(BulkUpdateErrorCounter counter, Map<String, String> cleanData,
      boolean successFlag, StringBuilder tempErrorMessage, String parameter) {
    if (cleanData.containsKey(parameter) && StringUtils.isEmpty(cleanData.get(parameter))) {
      successFlag = false;
      String errorMsg = getErrorMessage(parameter);
      boolean shouldAddToMessage = handleParameterSpecificLogic(counter, parameter);
      if (shouldAddToMessage) {
        tempErrorMessage.append(errorMsg).append(AND_SYMBOL);
      }
      LOGGER.error("Validation error : {} {} - error msg - {}", parameter, cleanData.get(parameter), errorMsg);
    }
    return successFlag;
  }


  private static boolean validateBlibliSkuAndProductSkuPartOfSeller(BulkUpdateErrorCounter counter,
      String businessPartnerCode, Map<String, String> cleanData, boolean successFlag, StringBuilder tempErrorMessage) {
    if (cleanData.containsKey(BulkParameters.BLIBLI_SKU) && StringUtils.isNotBlank(
        cleanData.get(BulkParameters.BLIBLI_SKU)) && !cleanData.get(BulkParameters.BLIBLI_SKU)
        .startsWith(businessPartnerCode)) {
      successFlag = false;
      counter.incrementBlibliSkuCounter();
      tempErrorMessage.append(
              String.format(BulkProcessValidationErrorMessages.INVALID_ITEM_SKU_ERROR, businessPartnerCode))
          .append(AND_SYMBOL);
    }
    if (cleanData.containsKey(BulkParameters.BLIBLI_PRODUCT_SKU) && StringUtils.isNotBlank(
        cleanData.get(BulkParameters.BLIBLI_PRODUCT_SKU)) && !cleanData.get(BulkParameters.BLIBLI_PRODUCT_SKU)
        .startsWith(businessPartnerCode)) {
      successFlag = false;
      counter.incrementBlibliProductSkuCounter();
      tempErrorMessage.append(
              String.format(BulkProcessValidationErrorMessages.INVALID_PRODUCT_SKU, businessPartnerCode))
          .append(AND_SYMBOL);
    }
    return successFlag;
  }

  private static String getErrorMessage(String parameter) {
    if (BulkParameters.BLIBLI_SKU.equals(parameter)) {
      return BLIBLI_SKU_BLANK;
    } else if (BulkParameters.BLIBLI_PRODUCT_SKU.equals(parameter)) {
      return BLIBLI_PRODUCT_SKU_BLANK;
    }
    return String.format(Constant.MANDATORY_FIELD_IS_BLANK, parameter);
  }

  private static boolean handleParameterSpecificLogic(BulkUpdateErrorCounter counter, String parameter) {
    if (BulkParameters.BLIBLI_SKU.equals(parameter)) {
      counter.incrementBlibliSkuCounter();
      return counter.getBlibliSkuCounter() <= ERROR_COUNT;
    } else if (BulkParameters.BLIBLI_PRODUCT_SKU.equals(parameter)) {
      counter.incrementBlibliProductSkuCounter();
      return counter.getBlibliProductSkuCounter() <= ERROR_COUNT;
    }
    // Default: always add to error message for generic parameters
    return true;
  }

  public static String getProductCodeFromItemCode(String itemCode) {
    int firstHyphenIndex = itemCode.indexOf(Constant.HYPHEN);
    int secondHyphenIndex = itemCode.indexOf(Constant.HYPHEN, firstHyphenIndex + 1);
    return itemCode.substring(0, secondHyphenIndex);
  }
}
