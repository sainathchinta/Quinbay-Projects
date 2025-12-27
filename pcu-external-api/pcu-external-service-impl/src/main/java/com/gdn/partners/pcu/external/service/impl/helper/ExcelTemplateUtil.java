package com.gdn.partners.pcu.external.service.impl.helper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFDataFormat;
import org.apache.poi.hssf.util.HSSFColor;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.SheetVisibility;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFColor;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.CnExcelHeaderInfo;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.ExcelHeaderNames;
import com.gdn.partners.pcu.external.service.impl.config.BeanUtilsConfigurer;
import com.gdn.partners.pcu.external.service.impl.config.ExcelHeaderHolder;
import com.gdn.partners.pcu.external.web.model.enums.MerchantType;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import com.gdn.partners.pcu.external.web.model.response.BulkSelectedDownloadHeaders;
import com.gdn.partners.pcu.external.web.model.response.ShippingTypeEligibility;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.google.cloud.storage.Blob;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

import lombok.extern.slf4j.Slf4j;
import net.sf.jxls.exception.ParsePropertyException;
import net.sf.jxls.transformer.XLSTransformer;

@Slf4j
public class ExcelTemplateUtil {

  private static final NumberFormat formatter = new DecimalFormat("0");
  private static final String COLOUR_FAMILY = "Family Colour";
  private static final String WARNA = "Warna";
  private static final String COLOR = "Color";
  private static final String GARANSI = "Garansi";
  private static final String WARRANTY = "Warranty";
  private static final String BRAND = "Brand";
  private static final String COLOR_WORKBOOK = "Values2";
  private static final String STORE_WORKBOOK = "Toko";
  private static final String COLOR_WORKBOOK_FOR_IS_MERCHANT = "COLOR_values";
  private static final String STORE_WORKBOOK_FOR_IS_MERCHANT = "Store";
  private static final String PRODUCT_INFO = "Info Produk";
  private static final String PRODUCT_INFO_EN = "Product Information";
  private static final String VARIENT = "Varian";
  private static final String VARIENT_EN = "Variants";
  private static final String IMAGES = "Gambar & Video";
  private static final String IMAGES_EN = "Images & Video";
  private static final String SHIPMENT_INFO = "Info pengiriman";
  private static final String SHIPMENT_INFO_EN = "Shipment Info";
  private static final String PRICE_AND_STOCK = "Harga & Stok";
  private static final String PRICE_AND_STOCK_EN = "Price & Stock";
  private static final String SKU_VISIBILITY_EN = "SKU Visibility";
  private static final String SKU_VISIBILITY = "Tampilan SKU";
  private static final String OTHER_ATTRIBUTES = "Atribut lainnya";
  private static final String OTHER_ATTRIBUTES_EN = "Other attributes";
  private static final String RECOMMENDED_ATTRIBUTES = "Rekomendasi atribut";
  private static final String RECOMMENDED_ATTRIBUTES_EN = "Recommended attributes";
  private static final String PREDEFINED_ATTRIBUTE = "predefinedAttribute";
  private static final String DEFINING_ATTRIBUTE = "definingAttribute";
  private static final String HEADER = "header";
  private static final String HEADER_PART = "headerPart";
  private static final String PICKUP_POINTS = "pickupPoints";
  private static final String CATEGORY = "Category";
  private static final String DOT = ".";
  private static final String XLS = ".xls";
  private static final String XLSX = ".xlsx";
  public static final int PRODUCT_INFO_HEADER = 5;
  public static final int IMAGES_VIDEO_HEADER = 8;
  public static final int SHIPMENT_INFO_HEADER = 6;
  public static final int PRICE_AND_STOCK_HEADER = 4;
  public static final int MAX_IMAGE_COUNT = 7;
  public static final int FIRST_HEADER_ROWNUM = 0;
  public static final int SECOND_HEADER_ROWNUM = 1;
  public static final int HEADER_MANDATORY_ROWNUM = 2;
  public static final int HEADER_INFO_ROWNUM = 3;
  public static final int FIRST_ROW = 4;
  public static final int LAST_ROW = 500000;
  public static final int BRAND_SHIFT_START_ROW = 1;
  public static final int BRAND_MAX_NUMBER = 25000;
  public static final String CATEGORY_ATTRIBUTE_SHEET_NAME = "attributes";
  public static final int CATEGORY_DEFINING_ATTRIBUTE_VALUE_SHEET = 5;
  private static final String BFB_VISIBILITY = "Info BFB";
  private static final String BFB_VISIBILITY_EN = "BFB info";
  public static final int BRAND_INDEX = 5;
  public static final int RGB_RED_VALUE_BFB_HEADER_1 = 244;
  public static final int RGB_GREEN_VALUE_BFB_HEADER_1 = 143;
  public static final int RGB_BLUE_VALUE_BFB_HEADER_1 = 74;
  public static final int RGB_RED_VALUE_BFB_HEADER_2 = 236;
  public static final int RGB_GREEN_VALUE_BFB_HEADER_2 = 181;
  public static final int RGB_BLUE_VALUE_BFB_HEADER_2 = 118;
  public static final int BFF_SELLER_TYPE_VALUE = 2;
  private static final int BUNDLING_COLUMN_NUMBER = 2;
  private static final Map<Boolean, String> BUNDLE_INFO =
      ImmutableMap.of(true, "Product Bundle Info", false, "Info gabungan produk");
  private static final Map<Boolean, String> CHILD_SKU = ImmutableMap.of(true, "Child SKU", false, "SKU child");
  private static final Map<Boolean, String> QUANTITY = ImmutableMap.of(true, "Quantity", false, "Jumlah");
  private static final Map<Boolean, String> CHILD_SKU_HEADER_INFO =
      ImmutableMap.of(true, "Use comma to separate child SKU (Ex: ABC 12345, DEF 56789", false,
          "Gunakan koma untuk memisahkan SKU child (Cth: ABC 12345, DEF 56789)");
  private static final Map<Boolean, String> QUANTITY_HEADER_INFO =
      ImmutableMap.of(true, "Use comma to separate quantity (Ex: 1, 2)", false,
          "Gunakan koma untuk memisahkan jumlah (Cth: 1, 2)");
  private static final Set<String> EXCEL_FILE_TYPES = Set.of(XLSX, XLS);


  public static final String TRANSFER_REQUEST = "TRANSFER_REQUEST";
  public static final String ASSEMBLY_REQUEST = "ASSEMBLY_REQUEST";
  public static final String WAREHOUSE = "Warehouse";
  private static final String[] BUNDLING_TYPE = {"Physical", "Virtual"};
  private static final int BUNDLING_TYPE_COLUMN = 4;
  private static final int WAREHOUSE_COLUMN_NO_FOR_TRANSFER_REQUEST = 4;
  private static final int WAREHOUSE_COLUMN_FOR_ASSEMBLY_DISASSEMBLY = 2;
  private static final int DATA_SHEET_INDEX = 0;
  private static final int WAREHOUSE_SHEET_INDEX = 1;
  private static final int FIRST_ROW_ASSEMBLY = 1;
  private static final int WAREHOUSE_CODE_NAME_COLUMN = 0;

  public static final ImmutableList<String> HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_ID =
      ImmutableList.of(ExcelHeaderNames.SKU_BLIBLI, ExcelHeaderNames.PICKUPPOINT_CODE_ID, ExcelHeaderNames.HARGA_NORMAL,
          ExcelHeaderNames.HARGA_JUAL, ExcelHeaderNames.STOK, ExcelHeaderNames.EXTERNAL_SKU_STATUS,
          ExcelHeaderNames.CNC_ACTIVE);

  public static final ImmutableList<String> HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_NON_CNC_ID =
      ImmutableList.of(ExcelHeaderNames.SKU_BLIBLI, ExcelHeaderNames.PICKUPPOINT_CODE_ID, ExcelHeaderNames.HARGA_NORMAL,
          ExcelHeaderNames.HARGA_JUAL, ExcelHeaderNames.STOK, ExcelHeaderNames.DELIVERY_ACTIVE);
  public static final ImmutableList<String> HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_EN =
      ImmutableList.of(ExcelHeaderNames.BLIBLI_SKU_EN, ExcelHeaderNames.PICKUP_POINT_CODE_EN, ExcelHeaderNames.PRICE,
          ExcelHeaderNames.SELLING_PRICE, ExcelHeaderNames.STOCK, ExcelHeaderNames.EXTERNAL_SKU_STATUS_EN,
          ExcelHeaderNames.CNC_ACTIVE);

  public static final ImmutableList<String> HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_NON_CNC_EN =
      ImmutableList.of(ExcelHeaderNames.BLIBLI_SKU_EN, ExcelHeaderNames.PICKUP_POINT_CODE_EN, ExcelHeaderNames.PRICE,
          ExcelHeaderNames.SELLING_PRICE, ExcelHeaderNames.STOCK, ExcelHeaderNames.DELIVERY_ACTIVE_EN_NEW);
  public static final ImmutableSet<String> HEADER_LIST_FOR_SELECTED_BULK_DOWNLOAD = ImmutableSet
      .of(ExcelHeaderNames.BRAND_CODE, ExcelHeaderNames.BRAND_NAME, ExcelHeaderNames.SELLER_CODE,
          ExcelHeaderNames.SELLER_NAME, ExcelHeaderNames.AUTH_START_DATE, ExcelHeaderNames.AUTH_END_DATE);

  public static final ImmutableList<String> HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_ID_CNC_1P =
      ImmutableList.of(ExcelHeaderNames.SKU_BLIBLI, ExcelHeaderNames.PICKUPPOINT_CODE_ID,
          ExcelHeaderNames.HARGA_NORMAL, ExcelHeaderNames.HARGA_JUAL, ExcelHeaderNames.STOK,
          ExcelHeaderNames.DELIVERY_ACTIVE, ExcelHeaderNames.CNC_ACTIVE);

  public static final ImmutableList<String> HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_EN_CNC_1P =
      ImmutableList.of(ExcelHeaderNames.BLIBLI_SKU_EN, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
          ExcelHeaderNames.PRICE, ExcelHeaderNames.SELLING_PRICE, ExcelHeaderNames.STOCK,
          ExcelHeaderNames.DELIVERY_ACTIVE_EN_NEW, ExcelHeaderNames.CNC_ACTIVE);

  public static final ImmutableList<String> INSTORE_FILE_OPTIONAL_HEADERS =
      ImmutableList.of(ExcelHeaderNames.DESCRIPTION, ExcelHeaderNames.DESKRIPSI, ExcelHeaderNames.LENGTH,
          ExcelHeaderNames.WIDTH, ExcelHeaderNames.HEIGHT, ExcelHeaderNames.WEIGHT, ExcelHeaderNames.PANJANG,
          ExcelHeaderNames.LEBAR, ExcelHeaderNames.TINGGI, ExcelHeaderNames.BERAT);

  private static final Map<Boolean, String> MANDATORY_DESCRIPTION_INFO =
      ImmutableMap.of(true, CnExcelHeaderInfo.MANDATORY_EN, false, CnExcelHeaderInfo.MANDATORY_ID);

  public static List<MultipartFile> getMultipartFiles(String username, List<String> filenames) throws IOException {
    List<MultipartFile> multipartFileList = new ArrayList<>();
    for (String fileName : filenames) {
      String originalFileName = Constants.DATA_BASE_DIR + username + Constants.ROOT + fileName;
      File file = new File(originalFileName);
      MockMultipartFile mockMultipartFile =
          new MockMultipartFile(fileName, originalFileName, null, new FileInputStream(file));
      multipartFileList.add(mockMultipartFile);
    }
    return multipartFileList;
  }

  public static Workbook generateWorkbookTemplateSelectedBrandBulk(BulkBrandDataRequest bulkBrandDataRequest) {
    Workbook workbook = POIUtil.generateWorkbookForBrandAuth(HEADER_LIST_FOR_SELECTED_BULK_DOWNLOAD, bulkBrandDataRequest);
    return workbook;
  }

  public static Workbook generateWorkbookTemplateBulkUpdate(ProfileResponse businessPartner, boolean isOnlyExternalUser,
      Map<String, Boolean> privilege, List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList,
      boolean multiPickupPointEnabled, boolean cncForWarehouseFeatureSwitch, int bulkUpdateTemplateColumnWidth,
      boolean preOrderQuotaFeatureSwitch) {
    boolean isBusinessPartnerO2O = businessPartner.getCompany().isOfflineToOnlineFlag();
    boolean isBusinessPartnerB2b = isB2bMerchant(businessPartner);
    boolean isBlibliFulfillment =
        businessPartner.getCompany().getInventoryFulfillment().equals(Constants.INVENTORY_FULFILLMENT_BLIBLI);
    boolean isOmgSeller = RequestHelper.getBusinessPartnerFlagValue(businessPartner, Constants.BLIBLI_OMG);
    Map<Integer, List<String>> products = new TreeMap<Integer, List<String>>();
    int index = 0;
    boolean isCncActiveRequired =
      multiPickupPointEnabled && checkSellerCncStatus(businessPartner);
    BulkSelectedDownloadHeaders bulkSelectedDownloadHeaders =
        getHeaders(isOnlyExternalUser, isBusinessPartnerO2O, isBlibliFulfillment, privilege,
            businessPartner.getCompany().getMerchantType(), isCncActiveRequired, isBusinessPartnerB2b,
            cncForWarehouseFeatureSwitch, isOmgSeller, preOrderQuotaFeatureSwitch);
    for (ProductLevel3SummaryResponse productLevel3SummaryResponse : productLevel3SummaryResponseList) {
      List<String> response = new ArrayList<String>();
      response.add(productLevel3SummaryResponse.getProductSku());
      response.add(productLevel3SummaryResponse.getProductName());
      response.add(productLevel3SummaryResponse.getItemSku());
      if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PICKUP_POINT))) {
        response.add(productLevel3SummaryResponse.getPickupPointCode());
        response.add(productLevel3SummaryResponse.getPickupPointName());
      }
      response.add(productLevel3SummaryResponse.getItemName());
      if (!isOnlyExternalUser) {
        response.add(productLevel3SummaryResponse.getSkuCode());
      }
      response.add(productLevel3SummaryResponse.getMerchantSku());

      if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE))) {
        response.add(formatter.format(productLevel3SummaryResponse.getPrices().get(0).getPrice()));
        response.add(formatter.format(productLevel3SummaryResponse.getOriginalSellingPrice()));
      }
      if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK))) {
        response.add((Boolean.TRUE.equals(productLevel3SummaryResponse.getSynchronizeStock())) ?
          "-" :
          String.valueOf(productLevel3SummaryResponse.getAvailableStockLevel2()));
        setPoQuota(response, productLevel3SummaryResponse, isOmgSeller, preOrderQuotaFeatureSwitch);
        response.add(
            formatter.format(Optional.ofNullable(productLevel3SummaryResponse.getMinimumStockLevel2()).orElse(0)));
      }

      if (cncForWarehouseFeatureSwitch) {
        ProductLevel3ViewConfigResponse defaultViewConfigResponse =
            getItemViewConfigByChannel(productLevel3SummaryResponse, Constants.DEFAULT_CHANNEL);
        String finalStatus = "0";
        String defaultStatus = setViewconfigValue(isOnlyExternalUser, defaultViewConfigResponse.getBuyable(),
            defaultViewConfigResponse.getDisplay());
        if (isCncActiveRequired) {
          ProductLevel3ViewConfigResponse cncViewConfigResponse =
              getItemViewConfigByChannel(productLevel3SummaryResponse, Constants.CNC_CHANNEL);
          String cncStatus = setViewconfigValue(isOnlyExternalUser, cncViewConfigResponse.getBuyable(),
              cncViewConfigResponse.getDisplay());
          if (ExcelHeaderNames.SKU_STATUS_OFFLINE.equals(cncStatus)) {
            finalStatus = defaultStatus;
          } else if (ExcelHeaderNames.SKU_STATUS_OFFLINE.equals(defaultStatus)) {
            finalStatus = cncStatus;
          } else if (defaultStatus.equals(cncStatus)) {
            finalStatus = defaultStatus;
          }
          if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE))) {
            response.add(finalStatus);
            response.add(ExcelHeaderNames.SKU_STATUS_OFFLINE.equals(defaultStatus) ?
                ExcelHeaderNames.SKU_STATUS_OFFLINE :
                ExcelHeaderNames.SKU_STATUS_ONLINE);
            response.add(ExcelHeaderNames.SKU_STATUS_OFFLINE.equals(cncStatus) ?
                ExcelHeaderNames.SKU_STATUS_OFFLINE :
                ExcelHeaderNames.SKU_STATUS_ONLINE);
          }
        } else {
          if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE))) {
            response.add(defaultStatus);
          }
        }
      } else {
        if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE))) {
          ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
              getItemViewConfigByChannel(productLevel3SummaryResponse, Constants.DEFAULT_CHANNEL);
          response.add(setViewconfigValue(isOnlyExternalUser, productLevel3ViewConfigResponse.getBuyable(),
              productLevel3ViewConfigResponse.getDisplay()));
        }
        if (isCncActiveRequired) {
          response.add(productLevel3SummaryResponse.isCncActive() ? "1" : "0");
        }
      }
      if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_O2O)) && isBusinessPartnerO2O) {
        response.add((productLevel3SummaryResponse.getOff2OnActiveFlag()) ? "1" : "0");
      }
      if ((!isOnlyExternalUser || isWareHouseMerchant(businessPartner.getCompany().getMerchantType())) && isBlibliFulfillment) {
        response.add(String.valueOf(productLevel3SummaryResponse.getAvailableStockLevel1() == null ?
            "-" :
            productLevel3SummaryResponse.getAvailableStockLevel1()));
      }
      if (isBusinessPartnerB2b) {
        response.add(setB2bBasePrice(productLevel3SummaryResponse));
        response.add(setB2bManagedFlag(productLevel3SummaryResponse));
        response.add(setBfbStatus(isOnlyExternalUser, productLevel3SummaryResponse));
      }
      products.put(index++, response);
    }
    int pickupPointIndex = bulkSelectedDownloadHeaders.getPickupPointIndex();
    Workbook workbook =
        POIUtil.generateXLFile(products, businessPartner.getPickupPoints(), isWhiteListedSeller(businessPartner),
            bulkSelectedDownloadHeaders, bulkUpdateTemplateColumnWidth);
    generateValidationWorkbookTemplateBulkUpdateSXSSF(workbook, businessPartner.getPickupPoints(), pickupPointIndex,
        privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PICKUP_POINT), "Toko!$A$1:$A$", 4);
    return workbook;
  }

  private static String setB2bManagedFlag(ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    return Objects.nonNull(productLevel3SummaryResponse.getB2BResponse()) && Objects.nonNull(
        productLevel3SummaryResponse.getB2BResponse().getBasePrice()) ?
        (productLevel3SummaryResponse.getB2BResponse().isManaged() ?
            ExcelHeaderNames.BFB_MANAGED_TRUE :
            ExcelHeaderNames.BFB_MANAGED_FALSE) :
        Constants.HYPHEN_VALUE;
  }

  private static String setB2bBasePrice(ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    return Objects.nonNull(productLevel3SummaryResponse.getB2BResponse()) && Objects.nonNull(
        productLevel3SummaryResponse.getB2BResponse().getBasePrice()) ?
        String.valueOf(productLevel3SummaryResponse.getB2BResponse().getBasePrice()) :
        Constants.HYPHEN_VALUE;
  }

  private static String setBfbStatus(boolean isOnlyExternalUser,
      ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    if (productLevel3SummaryResponse.getViewConfigs().stream()
        .anyMatch(viewConfigResponse -> Constants.B2B_CHANNEL.equals(viewConfigResponse.getChannelId()))) {
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
          getItemViewConfigByChannel(productLevel3SummaryResponse, Constants.B2B_CHANNEL);
      boolean display = BooleanUtils.toBooleanDefaultIfNull(productLevel3ViewConfigResponse.getDisplay(), false);
      boolean buyable = BooleanUtils.toBooleanDefaultIfNull(productLevel3ViewConfigResponse.getBuyable(), false);
      return setViewconfigValue(isOnlyExternalUser, buyable, display);
    } else {
      return Constants.HYPHEN_VALUE;
    }
  }

  private static ProductLevel3ViewConfigResponse getItemViewConfigByChannel(
      ProductLevel3SummaryResponse productLevel3SummaryResponse, String channel) {
    return productLevel3SummaryResponse.getViewConfigs().stream()
        .filter(viewConfigResponse -> channel.equals(viewConfigResponse.getChannelId())).findFirst()
        .orElse(new ProductLevel3ViewConfigResponse());
  }

  private static boolean isB2bMerchant(ProfileResponse businessPartner) {
    MerchantType merchantType = RequestHelper.getMerchantType(businessPartner);
    return merchantType.getType() > BFF_SELLER_TYPE_VALUE;
  }

  public static Workbook generateWorkbookTemplateMultiPickuppoint(ProfileResponse profileResponse,
      boolean cncForWarehouseFeatureSwitch) {
    Workbook workbook = null;
    int pickupPointColumn =
        HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_EN.indexOf(ExcelHeaderNames.PICKUP_POINT_CODE_EN);
    String formulaConstraint = StringUtils.EMPTY;
    if (profileResponse.getCompany().isInternationalFlag()) {
      formulaConstraint =
          Constants.SINGLE_QUOTE + ExcelHeaderNames.PICKUP_POINT_CODE_SHEET_EN + Constants.SINGLE_QUOTE + "!$A$2:$A$";
      if (profileResponse.getCompany().isCncActivated()) {
        workbook = POIUtil.generateWorkbookForMPPTemplate(
            getHeaders(profileResponse.getCompany().isInternationalFlag(),
                cncForWarehouseFeatureSwitch),
            profileResponse.getPickupPoints(), ExcelHeaderNames.PICKUP_POINT_CODE_SHEET_EN,
            ExcelHeaderNames.PICKUP_POINT_CODE_EN, ExcelHeaderNames.PICKUP_POINT_NAME_EN, isWhiteListedSeller(profileResponse));
      } else {
        workbook = POIUtil.generateWorkbookForMPPTemplate(HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_NON_CNC_EN,
            profileResponse.getPickupPoints(), ExcelHeaderNames.PICKUP_POINT_CODE_SHEET_EN,
            ExcelHeaderNames.PICKUP_POINT_CODE_EN, ExcelHeaderNames.PICKUP_POINT_NAME_EN, isWhiteListedSeller(profileResponse));
      }
      generateValidationWorkbookTemplateBulkUpdateSXSSF(workbook, profileResponse.getPickupPoints(), pickupPointColumn,
          Boolean.TRUE, formulaConstraint, 1);
    } else {
      formulaConstraint =
          Constants.SINGLE_QUOTE + ExcelHeaderNames.PICKUP_POINT_CODE_SHEET_ID + Constants.SINGLE_QUOTE + "!$A$2:$A$";
      if (profileResponse.getCompany().isCncActivated()) {
        workbook = POIUtil.generateWorkbookForMPPTemplate(
            getHeaders(profileResponse.getCompany().isInternationalFlag(),
                cncForWarehouseFeatureSwitch),
            profileResponse.getPickupPoints(), ExcelHeaderNames.PICKUP_POINT_CODE_SHEET_ID,
            ExcelHeaderNames.PICKUPPOINT_CODE_ID, ExcelHeaderNames.PICKUPPOINT_NAME_ID, isWhiteListedSeller(profileResponse));
      } else {
        workbook = POIUtil.generateWorkbookForMPPTemplate(HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_NON_CNC_ID,
            profileResponse.getPickupPoints(), ExcelHeaderNames.PICKUP_POINT_CODE_SHEET_ID,
            ExcelHeaderNames.PICKUPPOINT_CODE_ID, ExcelHeaderNames.PICKUPPOINT_NAME_ID, isWhiteListedSeller(profileResponse));
      }
      generateValidationWorkbookTemplateBulkUpdateSXSSF(workbook, profileResponse.getPickupPoints(), pickupPointColumn,
          Boolean.TRUE, formulaConstraint, 1);
    }
    return workbook;
  }

  private static ImmutableList<String> getHeaders(boolean isInternationalFlag,
      boolean cncForWarehouseFeatureSwitch) {
    if (isInternationalFlag) {
      return cncForWarehouseFeatureSwitch ?
          HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_EN_CNC_1P :
          HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_EN;
    } else {
      return cncForWarehouseFeatureSwitch ?
          HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_ID_CNC_1P :
          HEADER_LIST_FOR_MULTI_PICKUPPOINT_TEMPLATE_ID;
    }
  }

  private static boolean checkSellerCncStatus(ProfileResponse profileResponse) {
    return profileResponse.getCompany().isCncActivated();
  }

  public static boolean isWareHouseMerchant(String merchantType){
    return (Constants.MERCHANT_TYPE_TC.equals(merchantType) || Constants.MERCHANT_TYPE_CC.equals(merchantType)
        || Constants.MERCHANT_TYPE_TD.equals(merchantType));
  }

  private static String setViewconfigValue(boolean isOnlyExternalUser, Boolean buyable,
      Boolean display) {
    if (isOnlyExternalUser) {
      return (Boolean.TRUE.equals(buyable) && Boolean.TRUE.equals(display) ?
          ExcelHeaderNames.SKU_STATUS_ONLINE :
          ExcelHeaderNames.SKU_STATUS_OFFLINE);
    } else {
      if (Boolean.FALSE.equals(buyable) && Boolean.FALSE.equals(display)) {
        return ExcelHeaderNames.SKU_STATUS_OFFLINE;
      } else if (Boolean.FALSE.equals(buyable) && Boolean.TRUE.equals(display)) {
        return ExcelHeaderNames.SKU_STATUS_TEASER;
      } else if (Boolean.TRUE.equals(buyable) && Boolean.FALSE.equals(display)) {
        return ExcelHeaderNames.SKU_STATUS_B2B;
      } else if (Boolean.TRUE.equals(buyable) && Boolean.TRUE.equals(display)) {
        return ExcelHeaderNames.SKU_STATUS_ONLINE;
      } else {
        return ExcelHeaderNames.SKU_STATUS_OFFLINE;
      }
    }
  }

  private static void generateValidationWorkbookTemplateBulkUpdateSXSSF(Workbook workbook,
      List<PickupPointDTO> pickupPoints, int pickupPointIndex, Boolean isPrivilegedToEditPickupPoint,
      String formulaConstraint, int pickupPointDropDownStartIndex) {
    Sheet dataSheet = workbook.getSheet("Data");
    DataValidationHelper dataValidationHelper = null;
    DataValidationConstraint dataValidationConstraint = null;
    DataValidation dataValidation = null;
    if (Objects.nonNull(isPrivilegedToEditPickupPoint) && isPrivilegedToEditPickupPoint) {
      CellRangeAddressList cellRangeAddressList =
          new CellRangeAddressList(pickupPointDropDownStartIndex, 500000, pickupPointIndex, pickupPointIndex);
      dataValidationHelper = dataSheet.getDataValidationHelper();
      dataValidationConstraint =
          dataValidationHelper.createFormulaListConstraint(formulaConstraint + (pickupPoints.size() + 1));
      dataValidation = dataValidationHelper.createValidation(dataValidationConstraint, cellRangeAddressList);
      dataSheet.addValidationData(dataValidation);
    }
  }

  private static BulkSelectedDownloadHeaders getHeaders(boolean isOnlyExternalUser, boolean isBusinessPartnerO2O,
    boolean isBlibliFulfillment, Map<String, Boolean> privilege, String merchantType,
    boolean isCncActiveRequired, boolean isBusinessPartnerB2b, boolean cncForWarehouseFeatureSwitch,
      boolean isOmgSeller, boolean preOrderQuotaFeatureSwitch) {
    List<String> primaryHeaders = new ArrayList<>();
    List<String> secondaryHeaders = new ArrayList<>();
    List<String> tertiaryHeaders = new ArrayList<>();
    List<String> quaternaryHeaders = new ArrayList<>();
    int pickupPointIndex = 3;
    //Product sku column
    primaryHeaders.add(ExcelHeaderNames.BLIBLI_PRODUCT_SKU);
    secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
    tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
    quaternaryHeaders.add(ExcelHeaderNames.BLIBLI_PRODUCT_SKU_DESCRIPTION);

    //Parent product name column
    primaryHeaders.add(ExcelHeaderNames.PARENT_PRODUCT_NAME);
    secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
    tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
    quaternaryHeaders.add(ExcelHeaderNames.PARENT_PRODUCT_NAME_DESCRIPTION);
    //Item sku column
    primaryHeaders.add(ExcelHeaderNames.BLIBLI_SKU);
    secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
    tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
    quaternaryHeaders.add(ExcelHeaderNames.BLIBLI_SKU_DESCRIPTION);

    if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PICKUP_POINT)) && privilege
        .get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PICKUP_POINT)) {
      //Pickup point code column
      primaryHeaders.add(ExcelHeaderNames.TOKO_GUDANG);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.TOKO_GUDANG_DESCRIPTION);

      //Pickup point name column
      primaryHeaders.add(ExcelHeaderNames.PICKUP_POINT_NAME_COLUMN_ID);
      secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
      tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.PICKUP_POINT_NAME_COLUMN_DESCRIPTION);

    }
    //Product name column
    primaryHeaders.add(ExcelHeaderNames.NAMA_PRODUK);
    secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
    tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
    quaternaryHeaders.add(ExcelHeaderNames.NAMA_PRODUK_DESCRIPTION);

    if (!isOnlyExternalUser) {
      //Item code column
      primaryHeaders.add(ExcelHeaderNames.SKU_CODE);
      secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
      tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.SKU_CODE_DESCRIPTION);
    }
    //Seller sku column
    primaryHeaders.add(ExcelHeaderNames.SELLER_SKU);
    secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
    tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
    quaternaryHeaders.add(ExcelHeaderNames.SELLER_SKU_DESCRIPTION);
    if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE)) && privilege
        .get(Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE)) {
      //Normal price column
      primaryHeaders.add(ExcelHeaderNames.HARGA_RP);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.HARGA_RP_DESCRIPTION);

      //Selling price column
      primaryHeaders.add(ExcelHeaderNames.HARGA_PENJUALAN_RP);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.HARGA_PENJUALAN_RP_DESCRIPTION);
    }
    if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK)) && privilege
        .get(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK)) {
      //Stock column
      primaryHeaders.add(ExcelHeaderNames.STOK);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.STOK_DESCRIPTION);

      //PO Quota column
      if (isOmgSeller && preOrderQuotaFeatureSwitch) {
        primaryHeaders.add(ExcelHeaderNames.PO_QUOTA);
        secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
        tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
        quaternaryHeaders.add(ExcelHeaderNames.PO_QUOTA_DESCRIPTION);
      }

      //Min stock column
      primaryHeaders.add(ExcelHeaderNames.STOCK_REMINDER_COLUMN_ID);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.STOCK_REMINDER_COLUMN_DESCRIPTION_ID);

    }
    if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE)) && privilege.get(
        Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE)) {
      if (!cncForWarehouseFeatureSwitch) {
        if (!isOnlyExternalUser) {
          primaryHeaders.add(ExcelHeaderNames.AMPHI_SKU_STATUS);
          secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
          tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
          quaternaryHeaders.add(ExcelHeaderNames.AMPHI_SKU_STATUS_DESCRIPTION);

        } else {
          primaryHeaders.add(ExcelHeaderNames.EXTERNAL_SKU_STATUS);
          secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
          tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
          quaternaryHeaders.add(ExcelHeaderNames.EXTERNAL_SKU_STATUS_DESCRIPTION);

        }
      } else {
        if (!isOnlyExternalUser) {
          primaryHeaders.add(ExcelHeaderNames.AMPHI_SKU_STATUS_NEW);
          secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
          tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
          quaternaryHeaders.add(ExcelHeaderNames.AMPHI_SKU_STATUS_DESCRIPTION);

        } else {
          primaryHeaders.add(ExcelHeaderNames.EXTERNAL_SKU_STATUS_NEW);
          secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
          tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
          quaternaryHeaders.add(ExcelHeaderNames.EXTERNAL_SKU_STATUS_DESCRIPTION);

        }
      }
    }
    if (isCncActiveRequired) {
      //Delivery column
      primaryHeaders.add(ExcelHeaderNames.DELIVERY_ACTIVE_NEW);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.DELIVERY_ACTIVE_NEW_DESCRIPTION);

      //Cnc column
      primaryHeaders.add(ExcelHeaderNames.CNC_ACTIVE);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.CNC_ACTIVE_DESCRIPTION);

    }
    if (Objects.nonNull(privilege.get(Accessibilty.IS_PRIVILEGED_TO_EDIT_O2O)) && privilege
        .get(Accessibilty.IS_PRIVILEGED_TO_EDIT_O2O) && isBusinessPartnerO2O) {
      primaryHeaders.add(ExcelHeaderNames.IN_STORE);
      secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
      tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.IN_STORE_DESCRIPTION);

    }
    if ((!isOnlyExternalUser|| isWareHouseMerchant(merchantType)) && isBlibliFulfillment) {
      primaryHeaders.add(ExcelHeaderNames.WAREHOUSE_STOCK);
      secondaryHeaders.add(Constants.OPTIONAL_COLUMN_ID);
      tertiaryHeaders.add(Constants.NON_EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.WAREHOUSE_STOCK_DESCRIPTION);
    }
    if(isBusinessPartnerB2b){
      //Bfb price column
      primaryHeaders.add(ExcelHeaderNames.BFB_BASE_PRICE);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.BFB_BASE_PRICE_DESCRIPTION);

      // Bfb managed column
      primaryHeaders.add(ExcelHeaderNames.BFB_MANAGED);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.BFB_MANAGED_DESCRIPTION);

      // Bfb status column
      primaryHeaders.add(isOnlyExternalUser ? ExcelHeaderNames.EXTERNAL_BFB_STATUS : ExcelHeaderNames.AMPHI_BFB_STATUS);
      secondaryHeaders.add(Constants.MANDATORY_COLUMN_ID);
      tertiaryHeaders.add(Constants.EDITABLE_COLUMN_ID);
      quaternaryHeaders.add(ExcelHeaderNames.EXTERNAL_BFB_STATUS_DESCRIPTION);

    }
    BulkSelectedDownloadHeaders bulkSelectedDownloadHeaders = new BulkSelectedDownloadHeaders();
    bulkSelectedDownloadHeaders.setPrimaryHeaders(primaryHeaders);
    bulkSelectedDownloadHeaders.setSecondaryHeaders(secondaryHeaders);
    bulkSelectedDownloadHeaders.setTertiaryHeaders(tertiaryHeaders);
    bulkSelectedDownloadHeaders.setQuaternaryHeaders(quaternaryHeaders);
    bulkSelectedDownloadHeaders.setPickupPointIndex(pickupPointIndex);
    return bulkSelectedDownloadHeaders;
  }

  private static void setPoQuota(List<String> response, ProductLevel3SummaryResponse productLevel3SummaryResponse,
      boolean isOmgSeller, boolean preOrderQuotaFeatureSwitch) {
    if (isOmgSeller && preOrderQuotaFeatureSwitch) {
      PreOrderDTO preOrderDTO = PreOrderDTO.builder().isPreOrder(productLevel3SummaryResponse.isPreOrder())
          .preOrderDate(productLevel3SummaryResponse.getPreOrderDate()).build();
      if (isPreOrderActive(preOrderDTO)) {
        response.add(String.valueOf(productLevel3SummaryResponse.getAvailableStockLevel2()));
      } else {
        response.add(Constants.HYPHEN_VALUE);
      }
    }
  }

  public static void generateFileTemplate(String filename, Workbook workbook, HttpServletResponse servletResponse)
      throws Exception {
    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
    workbook.write(byteArrayOutputStream);
    BeanUtilsConfigurer.configure();
    byte[] bytes = byteArrayOutputStream.toByteArray();
    writeXlsFileContentToStream(servletResponse, bytes, filename);
  }

  private static void writeXlsFileContentToStream(HttpServletResponse httpServletResponse, byte[] fileContent,
      String fileName) throws IOException {
    httpServletResponse.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
    httpServletResponse.setHeader("Content-Disposition", "attachment; filename=" + fileName + ".xlsx");
    httpServletResponse.setContentLength(fileContent.length);
    httpServletResponse.getOutputStream().write(fileContent);
    httpServletResponse.getOutputStream().flush();
  }


  public static XSSFWorkbook getProductTemplate(CategoryDetailResponse category, ProfileResponse profileResponse,
      boolean isInternationalMerchant, List<AttributeResponse> definingAttributes,
      List<AttributeResponse> predefinedAttributes, Blob blob, MerchantType merchantType, boolean pickupPointNameConcat,
      String ppNameDelimiter, ShippingTypeEligibility shippingTypeEligibility, boolean isEligibleForBundlingColumns,
      boolean instoreNewFlowEnabled, boolean productSuitabilityFeatureEnabled) throws Exception {
    XSSFWorkbook workbook;
    try (ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(blob.getContent())) {
      List<PickupPointDTO> pickupPoints = profileResponse.getPickupPoints();
      boolean instoreEligible = RequestHelper.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, profileResponse);
      Map<String, List<?>> datas =
          downloadTemplate(category, pickupPoints, isInternationalMerchant, definingAttributes, predefinedAttributes,
              merchantType, isEligibleForBundlingColumns, instoreEligible, productSuitabilityFeatureEnabled);
      workbook = getProductTemplateWorkbook(datas, byteArrayInputStream);
      setProductTemplateData(datas, workbook, isInternationalMerchant, isWhiteListedSeller(profileResponse),
          merchantType, pickupPointNameConcat, ppNameDelimiter, shippingTypeEligibility, isEligibleForBundlingColumns,
          instoreEligible);
      setIdentifier(workbook, category.getCategoryCode());
    }
    return workbook;
  }

  private static void setIdentifier(XSSFWorkbook workbook, String categoryCode) {
    XSSFSheet categorySheet = workbook.createSheet(CATEGORY);
    workbook.setSheetVisibility(workbook.getSheetIndex(CATEGORY), SheetVisibility.VERY_HIDDEN);
    categorySheet.createRow(0).createCell(0).setCellValue(categoryCode);
  }

  private static Map<String, List<?>> downloadTemplate(CategoryDetailResponse category,
      List<PickupPointDTO> pickupPoints, boolean isInternationalMerchant, List<AttributeResponse> definingAttributes,
      List<AttributeResponse> predefinedAttributes, MerchantType merchantType, boolean isEligibleForBundlingColumns,
      boolean instoreEligible, boolean productSuitabilityFeatureEnabled) {
    List<Integer> headerPart = new ArrayList<Integer>();
    List<ExcelHeaderHolder> header = generateTemplateHeader(category, headerPart, isInternationalMerchant, merchantType,
        isEligibleForBundlingColumns, instoreEligible, productSuitabilityFeatureEnabled);
    List<AttributeResponse> attributes = definingAttributes;
    List<AttributeResponse> attributes2 = predefinedAttributes;
    Map<String, List<?>> datas = new HashMap<String, List<?>>();
    datas.put(HEADER, header);
    datas.put(HEADER_PART, headerPart);
    datas.put(DEFINING_ATTRIBUTE, attributes);
    datas.put(PREDEFINED_ATTRIBUTE, attributes2);
    datas.put(PICKUP_POINTS, pickupPoints);
    return datas;
  }

  private static List<ExcelHeaderHolder> generateTemplateHeader(CategoryDetailResponse category,
      List<Integer> headerPart, boolean isInternationalMerchant, MerchantType merchantType,
      boolean isEligibleForBundlingColumns, boolean instoreEligible, boolean productSuitabilityFeatureEnabled) {
    int bfbSkuVisibilityOffSet = 0;
    List<ExcelHeaderHolder> templateHeader = new ArrayList<>();
    if (isInternationalMerchant) {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.PRODUCT_NAME, true, CnExcelHeaderInfo.PRODUCT_NAME_EN));
      templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.DESCRIPTION, !instoreEligible, CnExcelHeaderInfo.DESCRIPTION_EN));
      templateHeader
          .add(1, new ExcelHeaderHolder(ExcelHeaderNames.MODEL_EAN_UPC, false, CnExcelHeaderInfo.MODEL_EAN_UPC_EN));
      templateHeader.add(2, new ExcelHeaderHolder(ExcelHeaderNames.SELLER_SKU, false, CnExcelHeaderInfo.SELLER_SKU_EN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.UNIQUE_SELLING_POINT, false,
          CnExcelHeaderInfo.UNIQUE_SELLING_POINT_EN));
    } else {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.NAMA_PRODUCK, true, CnExcelHeaderInfo.NAMA_PRODUCK_IN));
      templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.DESKRIPSI, !instoreEligible, CnExcelHeaderInfo.DESKRIPSI_IN));
      templateHeader
          .add(1, new ExcelHeaderHolder(ExcelHeaderNames.MODEL_EAN_UPC, false, CnExcelHeaderInfo.MODEL_EAN_UPC_IN));
      templateHeader.add(2, new ExcelHeaderHolder(ExcelHeaderNames.SELLER_SKU, false, CnExcelHeaderInfo.SELLER_SKU_IN));
      templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.KEUNGGULAN_PRODUK, false, CnExcelHeaderInfo.KEUNGGULAN_PRODUK_IN));
    }

    headerPart.add(PRODUCT_INFO_HEADER);
    ExcelHeaderHolder colourFamilyExcelHeaderHolder = null;

    int variationPart = 0;
    int otherAttributePart = 0;
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (org.apache.commons.lang3.StringUtils.equals(attributeName, COLOUR_FAMILY)) {
          if (isInternationalMerchant) {
            colourFamilyExcelHeaderHolder = new ExcelHeaderHolder(attributeName, false, CnExcelHeaderInfo.COLOUR_FAMILY_EN);
          } else {
            colourFamilyExcelHeaderHolder =
                new ExcelHeaderHolder(attributeName, false, CnExcelHeaderInfo.COLOUR_FAMILY_IN);
          }
        }
      }
    }

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && isDefiningOrVariantCreation(categoryAttribute)) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (isInternationalMerchant && Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        if (org.apache.commons.lang3.StringUtils.equals(attributeName, WARNA) || org.apache.commons.lang3.StringUtils
            .equals(attributeName, COLOR)) {
          if (isInternationalMerchant) {
            templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(),
                CnExcelHeaderInfo.COLOUR_EN));
          } else {
            templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(),
                CnExcelHeaderInfo.WARNA_IN));
          }
          templateHeader.add(colourFamilyExcelHeaderHolder);
          variationPart = variationPart + 2;
        } else {
          templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory()));
          variationPart++;
        }
      }
    }

    if (isInternationalMerchant) {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.PARENT, false, CnExcelHeaderInfo.PARENT_EN));
    } else {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.PARENT, false, CnExcelHeaderInfo.PARENT_IN));
    }
    variationPart++;
    if (isInternationalMerchant) {
      templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.VARIANT_IMAGE_EN, false, CnExcelHeaderInfo.VARIANT_IMAGE_INFO_EN));
    } else {
      templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.VARIANT_IMAGE_ID, false, CnExcelHeaderInfo.VARIANT_IMAGE_INFO_ID));
    }
    //Incrementing variationPart since variant image column is part of Variasi group header (primary group header)
    variationPart++;

    headerPart.add(PRODUCT_INFO_HEADER + variationPart);

    addImages(templateHeader, isInternationalMerchant);

    headerPart.add(PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER);
    addShipmentInfo(templateHeader, isInternationalMerchant, instoreEligible);

    headerPart.add(PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER);
    addPriceAndStock(templateHeader, isInternationalMerchant);

    int recommendedAttribute = 0;
    headerPart.add(PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER
        + PRICE_AND_STOCK_HEADER);
    //Using bfbSkuVisibilityOffSet to add incremental columns according to the merchant types
    if (merchantType.getType() > Constants.BFF_SELLER_TYPE_VALUE) {
      bfbSkuVisibilityOffSet = Constants.BFB_COLUMNS_COUNT;
      headerPart.add(
        PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER
          + PRICE_AND_STOCK_HEADER + Constants.BFB_COLUMNS_COUNT);
      addBfbInfo(templateHeader, isInternationalMerchant);
    }
    addSkuVisibilityInfo(templateHeader, isInternationalMerchant, merchantType);
    if(instoreEligible) {
      if (isInternationalMerchant) {
        templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.INSTORE_EN, true, "0",
            CnExcelHeaderInfo.INSTORE_INFO_EN));
      } else {
        templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.INSTORE_EN, true, "0",
            CnExcelHeaderInfo.INSTORE_INFO_ID));
      }
      variationPart++;
    }
    if (merchantType.getType() == MerchantType.CNC_SELLER.getType()
      || merchantType.getType() == MerchantType.BFB_CNC_SELLER.getType()) {
      bfbSkuVisibilityOffSet = bfbSkuVisibilityOffSet + Constants.CNC_COLUMNS_COUNT;
      headerPart.add(
        PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER
          + PRICE_AND_STOCK_HEADER + bfbSkuVisibilityOffSet);
    } else {
      bfbSkuVisibilityOffSet = bfbSkuVisibilityOffSet + Constants.PURE_DELIVERY_COLUMNS_COUNT;
      headerPart.add(
        PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER
          + PRICE_AND_STOCK_HEADER + bfbSkuVisibilityOffSet);
    }

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (RequestHelper.checkHideFromSellerAttribute(productSuitabilityFeatureEnabled, categoryAttribute)) {
        continue;
      }
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (isInternationalMerchant && Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        if (Objects.nonNull(attributeName) && (attributeName.contains(GARANSI) || attributeName.contains(WARRANTY))
            && categoryAttribute.getAttribute().isSkuValue()) {
          if (isInternationalMerchant) {
            templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(), CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_EN));
          } else {
            templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(), CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_IN));
          }
          recommendedAttribute++;
        } else if (!StringUtils.equalsIgnoreCase(BRAND, attributeName) && !StringUtils
            .equalsIgnoreCase(COLOUR_FAMILY, attributeName) && categoryAttribute.getAttribute().isBasicView()
            || categoryAttribute.getAttribute().isSkuValue()) {
          if (isInternationalMerchant) {
            templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(),
                CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_EN));
          } else {
            templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(),
                CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_IN));
          }
          recommendedAttribute++;
        }
      }
    }
    if (recommendedAttribute > 0) {
      headerPart.add(
        PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER
          + PRICE_AND_STOCK_HEADER + bfbSkuVisibilityOffSet + recommendedAttribute);
    }

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (RequestHelper.checkHideFromSellerAttribute(productSuitabilityFeatureEnabled, categoryAttribute)) {
        continue;
      }
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (isInternationalMerchant && Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        if (Objects.nonNull(attributeName) && !org.apache.commons.lang3.StringUtils
            .equals(attributeName, COLOUR_FAMILY)) {
          if (org.apache.commons.lang3.StringUtils.equals(categoryAttribute.getAttribute().getName(), BRAND)) {
            if (isInternationalMerchant) {
              templateHeader.add(BRAND_INDEX ,
                  new ExcelHeaderHolder(attributeName, true, CnExcelHeaderInfo.BRAND_EN));
            } else {
              templateHeader.add(BRAND_INDEX ,
                  new ExcelHeaderHolder(attributeName, true, CnExcelHeaderInfo.BRAND_IN));
            }
          } else if (!attributeName.contains(GARANSI) && !attributeName.contains(WARRANTY) && !categoryAttribute
              .getAttribute().isBasicView() && !categoryAttribute.getAttribute().isSkuValue()) {
            if (isInternationalMerchant) {
              templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(),
                  CnExcelHeaderInfo.OTHER_ATTRIBUTE_EN));
            } else {
              templateHeader.add(new ExcelHeaderHolder(attributeName, categoryAttribute.getAttribute().isMandatory(),
                  CnExcelHeaderInfo.OTHER_ATTRIBUTE_IN));
            }
            otherAttributePart++;
          }
        }
      }
    }

    if (otherAttributePart != 0) {
      headerPart.add(PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER
          + PRICE_AND_STOCK_HEADER + bfbSkuVisibilityOffSet + recommendedAttribute + otherAttributePart);
    }

    if (isEligibleForBundlingColumns) {
      templateHeader.add(new ExcelHeaderHolder(CHILD_SKU.get(isInternationalMerchant), false,
          CHILD_SKU_HEADER_INFO.get(isInternationalMerchant)));
      templateHeader.add(new ExcelHeaderHolder(QUANTITY.get(isInternationalMerchant), false,
          QUANTITY_HEADER_INFO.get(isInternationalMerchant)));
      headerPart.add(
          PRODUCT_INFO_HEADER + variationPart + IMAGES_VIDEO_HEADER + SHIPMENT_INFO_HEADER + PRICE_AND_STOCK_HEADER
              + bfbSkuVisibilityOffSet + recommendedAttribute + otherAttributePart + BUNDLING_COLUMN_NUMBER);
    }

    return templateHeader;
  }

  private static boolean isDefiningOrVariantCreation(CategoryAttributeResponse categoryAttributeResponse) {
    return AttributeType.DEFINING_ATTRIBUTE.name().equals(categoryAttributeResponse.getAttribute().getAttributeType())
        || categoryAttributeResponse.getAttribute().isVariantCreation();
  }

  private static boolean isDescriptiveOrPredefinedAndNotVariantCreation(
      CategoryAttributeResponse categoryAttributeResponse) {
    return
        (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(categoryAttributeResponse.getAttribute().getAttributeType())
            || AttributeType.PREDEFINED_ATTRIBUTE.name()
            .equals(categoryAttributeResponse.getAttribute().getAttributeType())) && !categoryAttributeResponse
            .getAttribute().isVariantCreation();
  }

  private static void addImages(List<ExcelHeaderHolder> templateHeader, boolean isInternationalMerchant) {
    if (isInternationalMerchant) {
      for (int i = 0; i < MAX_IMAGE_COUNT; i++) {
        if (i == 0) {
          templateHeader
              .add(new ExcelHeaderHolder(ExcelHeaderNames.IMAGE_PREFIX + (i + 1), true, CnExcelHeaderInfo.IMAGES_EN));
        } else {
          templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.IMAGE_PREFIX + (i + 1), false));
        }
      }
    } else {
      for (int i = 0; i < MAX_IMAGE_COUNT; i++) {
        if (i == 0) {
          templateHeader
              .add(new ExcelHeaderHolder(ExcelHeaderNames.FOTO_PREFIX + (i + 1), true, CnExcelHeaderInfo.FOTO_IN));
        } else {
          templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.FOTO_PREFIX + (i + 1), false));
        }
      }
    }
    if (isInternationalMerchant) {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.URL_VIDEO, false, CnExcelHeaderInfo.URL_VIDEO_EN));
    } else {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.URL_VIDEO, false, CnExcelHeaderInfo.URL_VIDEO_IN));
    }
  }

  private static void addShipmentInfo(List<ExcelHeaderHolder> templateHeader, boolean isInternationalMerchant,
      boolean instoreEligible) {
    if (isInternationalMerchant) {
      templateHeader
          .add(new ExcelHeaderHolder(ExcelHeaderNames.HANDLING_TYPE, true, CnExcelHeaderInfo.HANDLING_TYPE_EN));
      templateHeader
          .add(new ExcelHeaderHolder(ExcelHeaderNames.PICKUP_POINT_CODE, true, CnExcelHeaderInfo.PICKUP_POINT_CODE_EN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.LENGTH, !instoreEligible, CnExcelHeaderInfo.LENGTH_EN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.WIDTH, !instoreEligible, CnExcelHeaderInfo.WIDTH_EN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.HEIGHT, !instoreEligible, CnExcelHeaderInfo.HEIGHT_EN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.WEIGHT, !instoreEligible, CnExcelHeaderInfo.WEIGHT_EN));
    } else {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.TIPE_PENANGAN, true, CnExcelHeaderInfo.TIPE_PENANGAN_IN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.KODE_PICKUP_POINT, true, CnExcelHeaderInfo.KODE_PICKUP_POINT_IN));
      templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.PANJANG, !instoreEligible, CnExcelHeaderInfo.PANJANG_IN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.LEBAR, !instoreEligible, CnExcelHeaderInfo.LEBAR_IN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.TINGGI, !instoreEligible, CnExcelHeaderInfo.TINGGI_IN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.BERAT, !instoreEligible, CnExcelHeaderInfo.BERAT_IN));
    }
  }

  private static void addPriceAndStock(List<ExcelHeaderHolder> templateHeader, boolean isInternationalMerchant) {
    if (isInternationalMerchant) {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.PRICE, true, "0", CnExcelHeaderInfo.PRICE_EN));
      templateHeader
          .add(new ExcelHeaderHolder(ExcelHeaderNames.SELLING_PRICE, true, "0", CnExcelHeaderInfo.SELLING_PRICE_EN));
      templateHeader
          .add(new ExcelHeaderHolder(ExcelHeaderNames.AVAILABLE_STOCK, true, CnExcelHeaderInfo.AVAILABLE_STOCK_EN));
      templateHeader
          .add(new ExcelHeaderHolder(ExcelHeaderNames.MINIMUM_STOCK, true, CnExcelHeaderInfo.MINIMUM_STOCK_EN));
    } else {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.HARGA, true, "0", CnExcelHeaderInfo.HARGA_IN));
      templateHeader
          .add(new ExcelHeaderHolder(ExcelHeaderNames.HARGA_PENJULAN, true, "0", CnExcelHeaderInfo.HARGA_PENJULAN_IN));
      templateHeader
          .add(new ExcelHeaderHolder(ExcelHeaderNames.STOK_TERSEDIA, true, CnExcelHeaderInfo.STOK_TERSEDIA_IN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.STOK_MINIMUM, true, CnExcelHeaderInfo.STOK_MINIMUM_IN));
    }
  }

  private static void addSkuVisibilityInfo(List<ExcelHeaderHolder> templateHeader,
    boolean isInternationalMerchant, MerchantType merchantType) {
    if (isInternationalMerchant) {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.DELIVERY_STATUS, true, "0",
        CnExcelHeaderInfo.DELIVERY_STATUS_EN));
    } else {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.STATUS_PENGIRIMAN, true, "0",
        CnExcelHeaderInfo.STATUS_PENGIRIMAN_IN));
    }
    if (MerchantType.CNC_SELLER.equals(merchantType) || MerchantType.BFB_CNC_SELLER.equals(
      merchantType)) {
      if (isInternationalMerchant) {
        templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.CNC, true, "0", CnExcelHeaderInfo.CNC_STATUS_EN));
      } else {
        templateHeader.add(
          new ExcelHeaderHolder(ExcelHeaderNames.CNC, true, "0", CnExcelHeaderInfo.CNC_STATUS_IN));
      }
    }
  }

  private static void addBfbInfo(List<ExcelHeaderHolder> templateHeader,
    boolean isInternationalMerchant) {
    //Cell type 0 means INTEGER n 1 means STRING
    if (isInternationalMerchant) {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.BASE_PRICE_EN, true,"0",
        CnExcelHeaderInfo.BASE_PRICE_EN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.BFB_MANAGED_EN, true,"0",
        CnExcelHeaderInfo.MANAGED_STATUS));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.BFB_STATUS, true,"0",
        CnExcelHeaderInfo.STATUS_PENGIRIMAN_IN));
    } else {
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.BASE_PRICE_ID, true, "0",
        CnExcelHeaderInfo.BASE_PRICE_IN));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.BFB_MANAGED_ID, true, "0",
        CnExcelHeaderInfo.MANAGED_STATUS));
      templateHeader.add(new ExcelHeaderHolder(ExcelHeaderNames.BFB_STATUS, true, "0",
        CnExcelHeaderInfo.STATUS_PENGIRIMAN_IN));
    }
  }

  private static XSSFWorkbook getProductTemplateWorkbook(Map<String, List<?>> datas, InputStream is) throws Exception {
    XLSTransformer transformer = new XLSTransformer();
    return (XSSFWorkbook) transformer.transformXLS(is, datas);
  }

  /**
   * Fill the {@link Workbook}'s sheets with values of Map<br/>
   * Column headers will be filled with name of Product and Category attributes<br/>
   * Category's attributes are varying
   *
   * @param datas
   * @param workbook
   * @param isWhitelistedSeller
   * @param pickupPointNameConcat
   * @param ppNameDelimiter
   * @param instoreEligible
   * @throws ParsePropertyException
   * @throws InvalidFormatException
   */
  private static void setProductTemplateData(Map<String, List<?>> datas, XSSFWorkbook workbook,
      boolean isInternationalMerchant, boolean isWhitelistedSeller, MerchantType merchantType,
      boolean pickupPointNameConcat, String ppNameDelimiter, ShippingTypeEligibility shippingTypeEligibility,
      boolean isEligibleForBundlingColumns, boolean instoreEligible) {
    XSSFSheet dataSheet = workbook.getSheet("Data");
    XSSFSheet valuesSheet = workbook.getSheet(CATEGORY_ATTRIBUTE_SHEET_NAME);
    XSSFSheet pickupPointsSheet =
        isInternationalMerchant ? workbook.getSheet(STORE_WORKBOOK_FOR_IS_MERCHANT) : workbook.getSheet(STORE_WORKBOOK);
    setProductTemplateSheetHeader(datas, dataSheet, workbook, isInternationalMerchant, merchantType,
        shippingTypeEligibility, isEligibleForBundlingColumns, instoreEligible);
    setProductTemplatePredefinedValues(datas, valuesSheet, workbook);
    List<AttributeResponse> definingAttribute = (List<AttributeResponse>) datas.get("definingAttribute");
    if (!definingAttribute.isEmpty()) {
      XSSFSheet values2Sheet =
          isInternationalMerchant ? workbook.getSheet(COLOR_WORKBOOK_FOR_IS_MERCHANT) : workbook.getSheet(COLOR_WORKBOOK);
      setProductTemplateDefiningValues(values2Sheet, definingAttribute);
    }
    else {
      workbook.removeSheetAt(CATEGORY_DEFINING_ATTRIBUTE_VALUE_SHEET);
    }
    setProductTemplatePickupPoints(datas, pickupPointsSheet, isWhitelistedSeller, pickupPointNameConcat,
        ppNameDelimiter);
  }

  private static void setProductTemplateSheetHeader(Map<String, List<?>> datas, XSSFSheet dataSheet,
      XSSFWorkbook workbook, boolean isInternationalMerchant, MerchantType merchantType,
      ShippingTypeEligibility shippingTypeEligibility, boolean isEligibleForBundlingColumns, boolean instoreEligible) {
    List<Integer> headerPart = (List<Integer>) datas.get(HEADER_PART);
    int headerStyles = 7;
    String[] partValues;
    if (isInternationalMerchant) {
      partValues =
        new String[] {PRODUCT_INFO_EN, VARIENT_EN, IMAGES_EN, SHIPMENT_INFO_EN, PRICE_AND_STOCK_EN,
          SKU_VISIBILITY_EN, OTHER_ATTRIBUTES_EN};
    } else {
      partValues =
        new String[] {PRODUCT_INFO, VARIENT, IMAGES, SHIPMENT_INFO, PRICE_AND_STOCK, SKU_VISIBILITY,
          OTHER_ATTRIBUTES};
    }
    List<ExcelHeaderHolder> headerHolders = (List<ExcelHeaderHolder>) datas.get(HEADER);
    //Adding the template header values here
    if ((MerchantType.BFB_SELLER.equals(merchantType) || MerchantType.BFB_CNC_SELLER.equals(
      merchantType))) {
      if (isInternationalMerchant) {
        headerStyles = 8;
        partValues = new String[] {PRODUCT_INFO_EN, VARIENT_EN, IMAGES_EN, SHIPMENT_INFO_EN,
          PRICE_AND_STOCK_EN, BFB_VISIBILITY_EN, SKU_VISIBILITY_EN, OTHER_ATTRIBUTES_EN};
      } else {
        headerStyles = 8;
        partValues = new String[] {PRODUCT_INFO, VARIENT, IMAGES, SHIPMENT_INFO, PRICE_AND_STOCK,
          BFB_VISIBILITY, SKU_VISIBILITY, OTHER_ATTRIBUTES};
      }
      if (headerHolders.stream().map(ExcelHeaderHolder::getHeaderInfo).filter(Objects::nonNull)
        .anyMatch(headerInfo -> headerInfo.contains(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_EN))) {
        headerStyles = 9;
        partValues = new String[] {PRODUCT_INFO_EN, VARIENT_EN, IMAGES_EN, SHIPMENT_INFO_EN,
          PRICE_AND_STOCK_EN, BFB_VISIBILITY_EN, SKU_VISIBILITY_EN, RECOMMENDED_ATTRIBUTES_EN,
          OTHER_ATTRIBUTES_EN};
      } else if (headerHolders.stream().map(ExcelHeaderHolder::getHeaderInfo)
        .filter(Objects::nonNull)
        .anyMatch(headerInfo -> headerInfo.contains(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_IN))) {
        headerStyles = 9;
        partValues = new String[] {PRODUCT_INFO, VARIENT, IMAGES, SHIPMENT_INFO, PRICE_AND_STOCK,
          BFB_VISIBILITY, SKU_VISIBILITY, RECOMMENDED_ATTRIBUTES, OTHER_ATTRIBUTES};
      }
    } else {
      if (headerHolders.stream().map(ExcelHeaderHolder::getHeaderInfo).filter(Objects::nonNull)
        .anyMatch(headerInfo -> headerInfo.contains(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_EN))) {
        headerStyles = 8;
        partValues = new String[] {PRODUCT_INFO_EN, VARIENT_EN, IMAGES_EN, SHIPMENT_INFO_EN,
          PRICE_AND_STOCK_EN, SKU_VISIBILITY_EN, RECOMMENDED_ATTRIBUTES_EN, OTHER_ATTRIBUTES_EN};
      } else if (headerHolders.stream().map(ExcelHeaderHolder::getHeaderInfo)
        .filter(Objects::nonNull)
        .anyMatch(headerInfo -> headerInfo.contains(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_IN))) {
        headerStyles = 8;
        partValues = new String[] {PRODUCT_INFO, VARIENT, IMAGES, SHIPMENT_INFO, PRICE_AND_STOCK,
          SKU_VISIBILITY, RECOMMENDED_ATTRIBUTES, OTHER_ATTRIBUTES};
      }
    }

    boolean isOtherAttributes = headerHolders.stream().map(ExcelHeaderHolder::getHeaderInfo).filter(Objects::nonNull)
        .anyMatch(
            headerInfo -> ImmutableSet.of(CnExcelHeaderInfo.OTHER_ATTRIBUTE_EN, CnExcelHeaderInfo.OTHER_ATTRIBUTE_IN)
                .contains(headerInfo));

    boolean isRecommendedAttribute =
        headerHolders.stream().map(ExcelHeaderHolder::getHeaderInfo).filter(Objects::nonNull).anyMatch(
            headerInfo -> ImmutableSet.of(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_EN,
                CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_IN).contains(headerInfo));


    if (isEligibleForBundlingColumns) {
      if (isOtherAttributes) {
        partValues = ArrayUtils.add(partValues, BUNDLE_INFO.get(isInternationalMerchant));
        headerStyles = headerStyles + 1;
      } else {
        partValues[partValues.length - 1] = BUNDLE_INFO.get(isInternationalMerchant);
        headerStyles = headerStyles - 1;
      }
    }

    List<CellStyle> firstHeaderStyles =
        getFirstHeaderStyles(workbook, headerStyles, merchantType, isEligibleForBundlingColumns, isOtherAttributes,
            isRecommendedAttribute);
    List<CellStyle> secondHeaderStyles =
        getSecondHeaderStyles(workbook, headerStyles, merchantType, isEligibleForBundlingColumns, isOtherAttributes,
            isRecommendedAttribute);
    List<CellStyle> thirdHeaderStyles =
        getThirdHeaderStyles(workbook, headerStyles, merchantType, isEligibleForBundlingColumns, isOtherAttributes,
            isRecommendedAttribute);
    List<CellStyle> fourthHeaderStyles =
        getFourthHeaderStyles(workbook, headerStyles, merchantType, isEligibleForBundlingColumns, isOtherAttributes,
            isRecommendedAttribute);

    datas.remove(HEADER_PART);

    setValidation(dataSheet, (List<AttributeResponse>) datas.get(DEFINING_ATTRIBUTE),
        (List<AttributeResponse>) datas.get(PREDEFINED_ATTRIBUTE), (List<ExcelHeaderHolder>) datas.get(HEADER),
        (List<PickupPointDTO>) datas.get(PICKUP_POINTS), isInternationalMerchant, shippingTypeEligibility);

    int startIndex = 0;
    int partIndex = 0;
    dataSheet.createRow(FIRST_HEADER_ROWNUM);
    XSSFRow row1 = dataSheet.getRow(FIRST_HEADER_ROWNUM); // first header
    dataSheet.createRow(SECOND_HEADER_ROWNUM);
    XSSFRow row2 = dataSheet.getRow(SECOND_HEADER_ROWNUM); // second header
    dataSheet.createRow(HEADER_MANDATORY_ROWNUM);
    XSSFRow row3 = dataSheet.getRow(HEADER_MANDATORY_ROWNUM); // header optional/mandatory
    dataSheet.createRow(HEADER_INFO_ROWNUM);
    XSSFRow row4 = dataSheet.getRow(HEADER_INFO_ROWNUM); //header info
    List<ExcelHeaderHolder> excelHeaders = (List<ExcelHeaderHolder>) datas.get(HEADER);
    List<String> mandatoryColumns = new ArrayList<>();

    for (int i = 0; i < excelHeaders.size(); i++) {
      row1.createCell(i);
      row2.createCell(i);
      row3.createCell(i);
      row4.createCell(i);
      if (excelHeaders.get(i).isMandatory()) {
        row2.getCell(i).setCellValue(excelHeaders.get(i).getHeaderValue() + "*");
        row3.getCell(i).setCellValue(MANDATORY_DESCRIPTION_INFO.get(isInternationalMerchant));
      } else {
        row2.getCell(i).setCellValue(excelHeaders.get(i).getHeaderValue());
        if (isInternationalMerchant) {
          if(INSTORE_FILE_OPTIONAL_HEADERS.contains(excelHeaders.get(i).getHeaderValue()) && instoreEligible) {
            row2.getCell(i).setCellValue(excelHeaders.get(i).getHeaderValue() + "*");
            row3.getCell(i).setCellValue(CnExcelHeaderInfo.INSTORE_OPTIONAL_DESCRIPTION_EN);
          } else {
            row3.getCell(i).setCellValue("(Optional)");
          }
        } else {
          if(INSTORE_FILE_OPTIONAL_HEADERS.contains(excelHeaders.get(i).getHeaderValue()) && instoreEligible) {
            row2.getCell(i).setCellValue(excelHeaders.get(i).getHeaderValue() + "*");
            row3.getCell(i).setCellValue(CnExcelHeaderInfo.INSTORE_OPTIONAL_DESCRIPTION_ID);
          } else {
            row3.getCell(i).setCellValue("(Opsional)");
          }
        }
      }
      row4.getCell(i).setCellValue(excelHeaders.get(i).getHeaderInfo());
    }

    for (Integer part : headerPart) {
      for (int i = startIndex; i <= part; i++) {
        if(Objects.isNull(row1.getCell(i))) {
          continue;
        }
        row1.getCell(i).setCellStyle(firstHeaderStyles.get(partIndex));
        if (CellType.NUMERIC.name().equals(excelHeaders.get(i).getCellType())) {
          CellStyle numberFormat = workbook.createCellStyle();
          numberFormat.setDataFormat(HSSFDataFormat.getBuiltinFormat("#,##0.00"));
          dataSheet.setDefaultColumnStyle(i, numberFormat);
        }
        if (excelHeaders.get(i).isMandatory()) {
          if (!mandatoryColumns.contains((excelHeaders.get(i).getHeaderValue()))) {
            row2.getCell(i).setCellStyle(secondHeaderStyles.get(partIndex));
            mandatoryColumns.add(excelHeaders.get(i).getHeaderValue());
          }
        } else {
          row2.getCell(i).setCellStyle(secondHeaderStyles.get(partIndex));
        }
        row3.getCell(i).setCellStyle(thirdHeaderStyles.get(partIndex));
        row4.getCell(i).setCellStyle(fourthHeaderStyles.get(partIndex));
        if (mandatoryColumns.contains((excelHeaders.get(i).getHeaderValue())) && !instoreEligible) {
          dataSheet.addMergedRegion(new CellRangeAddress(1, 2, i, i));
        }
      }
      row1.getCell(startIndex).setCellValue(partValues[partIndex]);

      if (startIndex < part) {
        dataSheet.addMergedRegion(new CellRangeAddress(0, 0, startIndex, part));
        if (partIndex == 2) {
          dataSheet.addMergedRegion(new CellRangeAddress(3, 3, startIndex, (part - 1)));
          dataSheet.addMergedRegion(new CellRangeAddress(2, 2, (startIndex + 1), part));
          //part index means the 2nd header count that needs to be merged
        } else if (partIndex == 6 || partIndex == 7) {
          dataSheet.addMergedRegion(new CellRangeAddress(3, 3, startIndex, part));
        }
        startIndex = part + 1;
      } else if (startIndex == part) {
        startIndex = part + 1;
      }
      partIndex++;
    }

    List<ExcelHeaderHolder> totalHeaders = (List<ExcelHeaderHolder>) datas.get(HEADER);
    for (int i = 0; i < totalHeaders.size(); i++) {
      dataSheet.autoSizeColumn(i, true);
    }
  }

  /**
   * Set predefined values of product-template.xls<br/>
   * Values will be set in "Values" sheet
   *
   * @param datas
   * @param valuesSheet
   */
  @SuppressWarnings("unchecked")
  private static void setProductTemplatePredefinedValues(Map<String, List<?>> datas, XSSFSheet valuesSheet,
      XSSFWorkbook workbook) {
    int columnIndex = 0;
    List<String> dataList;
    valuesSheet.createRow(0);
    for (AttributeResponse predefinedAttribute : (List<AttributeResponse>) datas.get(PREDEFINED_ATTRIBUTE)) {
      dataList = new ArrayList<>();
      if (!BRAND.equalsIgnoreCase(predefinedAttribute.getName())) {
        valuesSheet.getRow(0).createCell(columnIndex).setCellValue(predefinedAttribute.getName());
        getProductPredifinedValues(dataList, predefinedAttribute, valuesSheet, columnIndex, workbook);
        valuesSheet.autoSizeColumn(columnIndex);
        columnIndex++;
      }
    }
  }

  private static CellStyle getDefaultBrandTextStyle(XSSFWorkbook workbook) {
    CellStyle style = workbook.createCellStyle();
    style.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    return style;
  }

  private static void getProductPredifinedValues(List<String> dataList, AttributeResponse predefinedAttribute,
      XSSFSheet valuesSheet, int columnIndex, XSSFWorkbook workbook) {
    int i = 1;
    for (PredefinedAllowedAttributeValueResponse value : predefinedAttribute.getPredefinedAllowedAttributeValues()) {
      dataList.add(value.getValue());
    }
    if (!BRAND.equals(predefinedAttribute.getName())) {
      Collections.sort(dataList, String.CASE_INSENSITIVE_ORDER);
    }
    if (BRAND.equals(predefinedAttribute.getName())) {
      int lastRowNum = valuesSheet.getLastRowNum();
      valuesSheet.shiftRows(BRAND_SHIFT_START_ROW, lastRowNum, dataList.size());
      for (String colValue : dataList) {
        XSSFRow row = valuesSheet.getRow(i);
        if (row == null) {
          valuesSheet.createRow(i);
          row = valuesSheet.getRow(i);
        }
        row.createCell(columnIndex);
        if (Constants.OEM.equalsIgnoreCase(colValue) || Constants.NO_BRAND.equalsIgnoreCase(colValue)) {
          row.getCell(columnIndex).setCellStyle(getDefaultBrandTextStyle(workbook));
        }
        row.getCell(columnIndex).setCellValue(colValue);
        i++;
      }
    } else {
      for (String colValue : dataList) {
        XSSFRow row = valuesSheet.getRow(i);
        if (row == null) {
          valuesSheet.createRow(i);
          row = valuesSheet.getRow(i);
        }
        row.createCell(columnIndex);
        row.getCell(columnIndex).setCellValue(colValue);
        i++;
      }
    }
  }

  private static void setProductTemplateDefiningValues(XSSFSheet valuesSheet, List<AttributeResponse> definingAttributes) {
    int i = 0;
    for (AttributeResponse definingAttribute : definingAttributes) {
      XSSFRow row = valuesSheet.getRow(0);
      if (row == null) {
        valuesSheet.createRow(0);
        row = valuesSheet.getRow(0);
      }
      row.createCell(i);
      row.getCell(i).setCellValue(definingAttribute.getName());
      int j = 1;
      List<AllowedAttributeValueResponse> allowedAttributeValues =
          definingAttribute.getAllowedAttributeValues().stream()
              .filter(value -> StringUtils.isNotBlank(value.getValue()))
              .collect(Collectors.toList());
      for (AllowedAttributeValueResponse value : sortAllowedAttributeValueResponse(allowedAttributeValues)) {
        XSSFRow rowValue = valuesSheet.getRow(j);
        if (rowValue == null) {
          valuesSheet.createRow(j);
          rowValue = valuesSheet.getRow(j);
        }
        rowValue.createCell(i);
        rowValue.getCell(i).setCellValue(value.getValue());
        j++;
      }
      valuesSheet.autoSizeColumn(i);
      i++;
    }
  }

  private static List<AllowedAttributeValueResponse> sortAllowedAttributeValueResponse(
      List<AllowedAttributeValueResponse> allowedAttributeValues) {
    Collections.sort(allowedAttributeValues, (o1, o2) -> {
      Integer s1 = o1.getSequence();
      Integer s2 = o2.getSequence();
      int sComp = s1.compareTo(s2);
      if (sComp != 0) {
        return sComp;
      } else {
        String v1 = o1.getValue();
        String v2 = o2.getValue();
        return v1.compareTo(v2);
      }
    });
    return allowedAttributeValues;
  }

  public static void setProductTemplatePickupPoints(Map<String, List<?>> datas, XSSFSheet pickupPointsSheet,
      boolean isWhitelistedSeller, boolean pickupPointNameConcat, String ppNameDelimiter) {
    int i = 0;
    for (PickupPointDTO pickupPoint : (List<PickupPointDTO>) datas.get(PICKUP_POINTS)) {
      XSSFRow row = pickupPointsSheet.getRow(i);
      if (row == null) {
        pickupPointsSheet.createRow(i);
        row = pickupPointsSheet.getRow(i);
      }
      row.createCell(0);
      row.createCell(1);
      row.getCell(0).setCellValue(pickupPoint.getCode());
      XSSFCell cell1 = null;
      if (pickupPointNameConcat) {
        StringBuilder ppCodeAndName = new StringBuilder();
        ppCodeAndName.append(pickupPoint.getCode()).append(StringUtils.SPACE).append(ppNameDelimiter)
            .append(StringUtils.SPACE).append(pickupPoint.getName());
        row.getCell(0).setCellValue(ppCodeAndName.toString());
      } else {
        cell1 = row.getCell(1);
        cell1.setCellValue(pickupPoint.getName());
      }
      if (isWhitelistedSeller && pickupPoint.isFbbActivated()) {
        int columnIndex = pickupPointNameConcat ? 1 : 2;
        row.createCell(columnIndex);
        row.getCell(columnIndex).setCellValue(Constants.FBB);
        pickupPointsSheet.autoSizeColumn(columnIndex);
      }
      i++;
    }
    pickupPointsSheet.autoSizeColumn(0);
    pickupPointsSheet.autoSizeColumn(1);
  }

  private static void setValidation(XSSFSheet sheet, List<AttributeResponse> attributes,
      List<AttributeResponse> predefinedAttributes, List<ExcelHeaderHolder> headers, List<PickupPointDTO> pickupPoints,
      boolean isInternationalMerchant, ShippingTypeEligibility shippingTypeEligibility) {
    setDropdownOption(sheet, headers, isInternationalMerchant, shippingTypeEligibility);
    setAttributeValidation(sheet, attributes, headers, isInternationalMerchant);
    setPredefinedAttributeValidation(sheet, predefinedAttributes, headers, isInternationalMerchant);
    setPickupPointsValidation(sheet, pickupPoints, headers, isInternationalMerchant);
  }

  private static void setDropdownOption(XSSFSheet sheet, List<ExcelHeaderHolder> headers,
      boolean isInternationalMerchant, ShippingTypeEligibility shippingTypeEligibility) {
    String shippingHeaderName =
      isInternationalMerchant ? ExcelHeaderNames.HANDLING_TYPE : ExcelHeaderNames.TIPE_PENANGAN;
    int positionShippingType = getHeaderPosition(headers, shippingHeaderName);
    CellRangeAddressList addressListShippingType =
        new CellRangeAddressList(FIRST_ROW, LAST_ROW, positionShippingType, positionShippingType);
    DataValidationHelper dvValidatorShippingType = new XSSFDataValidationHelper(sheet);
    DataValidationConstraint dvConstraintShippingType =
      dvValidatorShippingType.createExplicitListConstraint(
        getShippingTypeDropdownValues(isInternationalMerchant, shippingTypeEligibility).toArray(
          String[]::new));
    DataValidation dataValidationShippingType =
        dvValidatorShippingType.createValidation(dvConstraintShippingType, addressListShippingType);
    sheet.addValidationData(dataValidationShippingType);
  }

  private static List<String> getShippingTypeDropdownValues(boolean isInternationalMerchant,
    ShippingTypeEligibility shippingTypeEligibility) {
    return isInternationalMerchant ? setShippingTypeForInternationalMerchant(shippingTypeEligibility) :
      setShippingTypeForNonInternationalMerchant(shippingTypeEligibility);
  }

  private static List<String> setShippingTypeForInternationalMerchant(
    ShippingTypeEligibility shippingTypeEligibility) {
    List<String> shippingTypeList = new ArrayList<>();
    shippingTypeList.add(ExcelHeaderNames.THROUGH_BLIBLI_LOGISTIC_PARTNER);
    if (shippingTypeEligibility.isEligibleForBigProduct()) {
      shippingTypeList.add(ExcelHeaderNames.SHIPPED_BY_SELLER);
    }
    if (shippingTypeEligibility.isEligibleForBopisProduct()) {
      shippingTypeList.add(ExcelHeaderNames.BOPIS);
    }
    return shippingTypeList;
  }

  private static List<String> setShippingTypeForNonInternationalMerchant(
    ShippingTypeEligibility shippingTypeEligibility) {
    List<String> shippingTypeList = new ArrayList<>();
    shippingTypeList.add(ExcelHeaderNames.MELALUI_PARTNER_LOGISTIK_BLIBLI);
    if (shippingTypeEligibility.isEligibleForBigProduct()) {
      shippingTypeList.add(ExcelHeaderNames.DIKIRIMKAN_OLEH_SELLER);
    }
    if (shippingTypeEligibility.isEligibleForBopisProduct()) {
      shippingTypeList.add(ExcelHeaderNames.BOPIS);
    }
    return shippingTypeList;
  }


  private static int getHeaderPosition(List<ExcelHeaderHolder> headers, String headerName) {
    int i = 0;
    for (ExcelHeaderHolder data : headers) {
      if (data.getHeaderValue().equals(headerName)) {
        return i;
      }
      i++;
    }
    return -1;
  }

  private static void setAttributeValidation(XSSFSheet sheet, List<AttributeResponse> attributes,
      List<ExcelHeaderHolder> headers, boolean isInternationalMerchant) {
    int indexAscii = 65;
    for (AttributeResponse attribute : attributes) {
      int position = getHeaderPosition(headers, attribute.getName());
      if (isInternationalMerchant && Objects.nonNull(attribute.getNameEnglish())) {
        position = getHeaderPosition(headers, attribute.getNameEnglish());
      }
      List<String> allowedAttributeValues = new ArrayList<String>();
      for (AllowedAttributeValueResponse allowedAttributeValue : attribute.getAllowedAttributeValues()) {
        allowedAttributeValues.add(allowedAttributeValue.getValue());
      }
      CellRangeAddressList addressList = new CellRangeAddressList(FIRST_ROW, LAST_ROW, position, position);
      DataValidationConstraint dvConstraint;
      DataValidationHelper validationHelper = new XSSFDataValidationHelper(sheet);
      String attributeName = attribute.getName();
      if (isInternationalMerchant && Objects.nonNull(attribute.getNameEnglish())) {
        attributeName = attribute.getNameEnglish();
      }
      if (WARNA.equalsIgnoreCase(attributeName) || COLOR.equalsIgnoreCase(attributeName)) {
        if (isInternationalMerchant) {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "Color_values!$" + (char) indexAscii + "$2:$" + (char) indexAscii + "$" + (
                  attribute.getAllowedAttributeValues().size() + 50));
        } else {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "Values2!$" + (char) indexAscii + "$2:$" + (char) indexAscii + "$" + (
                  attribute.getAllowedAttributeValues().size() + 50));
        }
      } else {
        if (isInternationalMerchant) {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "Color_values!$" + (char) indexAscii + "$2:$" + (char) indexAscii + "$" + (
                  attribute.getAllowedAttributeValues().size() + 1));
        } else {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "Values2!$" + (char) indexAscii + "$2:$" + (char) indexAscii + "$" + (
                  attribute.getAllowedAttributeValues().size() + 1));
        }
      }
      DataValidation dataValidation = validationHelper.createValidation(dvConstraint, addressList);
      sheet.addValidationData(dataValidation);
      indexAscii++;
    }
  }

  private static void setPredefinedAttributeValidation(XSSFSheet sheet, List<AttributeResponse> predefinedAttributes,
      List<ExcelHeaderHolder> headers, boolean isInternationalMerchant) {
    int brandIndexAscii = 65;
    int indexAscii = 65;
    for (AttributeResponse attribute : predefinedAttributes) {
      int position = getHeaderPosition(headers, attribute.getName());
      if (isInternationalMerchant && Objects.nonNull(attribute.getNameEnglish())) {
        position = getHeaderPosition(headers, attribute.getNameEnglish());
      }
      List<String> predefinedAllowedAttributeValues = new ArrayList<String>();
      if (!BRAND.equals(attribute.getName())) {
        for (PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue : attribute.getPredefinedAllowedAttributeValues()) {
          predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValue.getValue());
        }
      }
      CellRangeAddressList addressList = new CellRangeAddressList(FIRST_ROW, LAST_ROW, position, position);
      DataValidationConstraint dvConstraint;
      DataValidationHelper validationHelper = new XSSFDataValidationHelper(sheet);
      String attributeName = attribute.getName();
      if (isInternationalMerchant && Objects.nonNull(attribute.getNameEnglish())) {
        attributeName = attribute.getNameEnglish();
      }
      if (BRAND.equalsIgnoreCase(attributeName)) {
        if (isInternationalMerchant) {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "Brand_values!$" + (char) brandIndexAscii + "$2:$" + (char) brandIndexAscii + "$" + (BRAND_MAX_NUMBER + LAST_ROW));
        } else {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "Values!$" + (char) brandIndexAscii + "$2:$" + (char) brandIndexAscii + "$" + (BRAND_MAX_NUMBER + LAST_ROW));
        }
      } else {
        if (isInternationalMerchant) {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "attributes!$" + (char) indexAscii + "$2:$" + (char) indexAscii + "$" + (
                  attribute.getPredefinedAllowedAttributeValues().size() + 1));
        } else {
          dvConstraint = validationHelper.createFormulaListConstraint(
              "attributes!$" + (char) indexAscii + "$2:$" + (char) indexAscii + "$" + (
                  attribute.getPredefinedAllowedAttributeValues().size() + 1));
        }
        indexAscii++;
      }
      DataValidation dataValidation = validationHelper.createValidation(dvConstraint, addressList);
      sheet.addValidationData(dataValidation);
    }
  }

  private static void setPickupPointsValidation(XSSFSheet sheet, List<PickupPointDTO> pickupPoints,
      List<ExcelHeaderHolder> headers, boolean isInternationalMerchant) {
    String pickupPointCodeHeaderName = isInternationalMerchant ? ExcelHeaderNames.PICKUP_POINT_CODE : ExcelHeaderNames.KODE_PICKUP_POINT;
    String storeHeaderName = isInternationalMerchant ? "Store!$A$1:$A$" : "Toko!$A$1:$A$";
    int position = getHeaderPosition(headers, pickupPointCodeHeaderName);
    List<String> pickupPointAllowedValues = new ArrayList<String>();
    for (PickupPointDTO pickupPoint : pickupPoints) {
      pickupPointAllowedValues.add(pickupPoint.getCode());
    }
    CellRangeAddressList addressList = new CellRangeAddressList(FIRST_ROW, LAST_ROW, position, position);
    DataValidationHelper dataValidationHelper = new XSSFDataValidationHelper(sheet);
    DataValidationConstraint dvConstraint =
        dataValidationHelper.createFormulaListConstraint(storeHeaderName + (pickupPointAllowedValues.size()));
    DataValidation dataValidation = dataValidationHelper.createValidation(dvConstraint, addressList);
    sheet.addValidationData(dataValidation);
  }

  private static List<CellStyle> getFirstHeaderStyles(XSSFWorkbook workbook, int headerStyles,
    MerchantType merchantType, boolean isEligibleForBundlingColumns, boolean isOtherAttributes, boolean isRecommendedAttributes) {
    // Cell style
    XSSFCellStyle style1 = workbook.createCellStyle();
    style1.setFillForegroundColor(new XSSFColor(new java.awt.Color(84, 130, 53)));
    commonCellStyleFirstHeader(workbook, style1, HSSFColor.BLACK.index);

    XSSFCellStyle style2 = workbook.createCellStyle();
    style2.setFillForegroundColor(new XSSFColor(new java.awt.Color(191, 143, 0)));
    commonCellStyleFirstHeader(workbook, style2, HSSFColor.BLACK.index);

    XSSFCellStyle style3 = workbook.createCellStyle();
    style3.setFillForegroundColor(new XSSFColor(new java.awt.Color(46, 117, 181)));
    commonCellStyleFirstHeader(workbook, style3, HSSFColor.WHITE.index);

    XSSFCellStyle style4 = workbook.createCellStyle();
    style4.setFillForegroundColor(new XSSFColor(new java.awt.Color(198, 89, 17)));
    commonCellStyleFirstHeader(workbook, style4, HSSFColor.BLACK.index);

    XSSFCellStyle style5 = workbook.createCellStyle();
    style5.setFillForegroundColor(new XSSFColor(new java.awt.Color(48, 82, 150)));
    commonCellStyleFirstHeader(workbook, style5, HSSFColor.WHITE.index);

    XSSFCellStyle style8 = workbook.createCellStyle();
    style8.setFillForegroundColor(new XSSFColor(
        new java.awt.Color(RGB_RED_VALUE_BFB_HEADER_1, RGB_GREEN_VALUE_BFB_HEADER_1, RGB_BLUE_VALUE_BFB_HEADER_1)));
    commonCellStyleFirstHeader(workbook, style8, HSSFColor.BLACK.index);

    List<CellStyle> styles = new ArrayList<CellStyle>();
    styles.add(style1);
    styles.add(style2);
    styles.add(style3);
    styles.add(style4);
    styles.add(style5);
    if (merchantType.getType() > BFF_SELLER_TYPE_VALUE) {
      styles.add(style8);
    }
    styles.add(style1);

    if (isRecommendedAttributes) {
      XSSFCellStyle style6 = workbook.createCellStyle();
      style6.setFillForegroundColor(new XSSFColor(new java.awt.Color(237, 125, 49)));
      commonCellStyleFirstHeader(workbook, style6, HSSFColor.BLACK.index);
      styles.add(style6);
    }

    if (isOtherAttributes) {
      XSSFCellStyle style7 = workbook.createCellStyle();
      style7.setFillForegroundColor(new XSSFColor(new java.awt.Color(123, 123, 123)));
      commonCellStyleFirstHeader(workbook, style7, HSSFColor.BLACK.index);
      styles.add(style7);
    }

    if (isEligibleForBundlingColumns) {
      styles.add(style8);
    }

    return styles;
  }

  private static List<CellStyle> getSecondHeaderStyles(XSSFWorkbook workbook, int headerStyles,
    MerchantType merchantType, boolean isEligibleForBundlingColumns, boolean isOtherAttributes,  boolean isRecommendedAttributes) {
    Font font = workbook.createFont();
    font.setBold(true);
    // Cell style
    XSSFCellStyle style1 = workbook.createCellStyle();
    style1.setFillForegroundColor(new XSSFColor(new java.awt.Color(169, 208, 142)));
    commonCellStyle(font, style1);

    XSSFCellStyle style2 = workbook.createCellStyle();
    style2.setFillForegroundColor(new XSSFColor(new java.awt.Color(255, 217, 102)));
    commonCellStyle(font, style2);

    XSSFCellStyle style3 = workbook.createCellStyle();
    style3.setFillForegroundColor(new XSSFColor(new java.awt.Color(155, 194, 230)));
    commonCellStyle(font, style3);

    XSSFCellStyle style4 = workbook.createCellStyle();
    style4.setFillForegroundColor(new XSSFColor(new java.awt.Color(244, 176, 132)));
    commonCellStyle(font, style4);

    XSSFCellStyle style5 = workbook.createCellStyle();
    style5.setFillForegroundColor(new XSSFColor(new java.awt.Color(142, 169, 219)));
    commonCellStyle(font, style5);

    XSSFCellStyle style8 = workbook.createCellStyle();
    style8.setFillForegroundColor(new XSSFColor(new java.awt.Color(RGB_RED_VALUE_BFB_HEADER_2,
      RGB_GREEN_VALUE_BFB_HEADER_2, RGB_BLUE_VALUE_BFB_HEADER_2)));
    commonCellStyle(font, style8);

    List<CellStyle> styles = new ArrayList<CellStyle>();
    styles.add(style1);
    styles.add(style2);
    styles.add(style3);
    styles.add(style4);
    styles.add(style5);
    //BFF n BFF CNC Merchant Type is greater than 2
    if (merchantType.getType() > BFF_SELLER_TYPE_VALUE) {
      styles.add(style8);
    }

    styles.add(style1);


    if (isRecommendedAttributes) {
      XSSFCellStyle style6 = workbook.createCellStyle();
      style6.setFillForegroundColor(new XSSFColor(new java.awt.Color(255, 192, 0)));
      commonCellStyle(font, style6);

      styles.add(style6);
    }

    if (isOtherAttributes) {
      XSSFCellStyle style7 = workbook.createCellStyle();
      style7.setFillForegroundColor(new XSSFColor(new java.awt.Color(201, 201, 201)));
      commonCellStyle(font, style7);
      styles.add(style7);
    }


    if (isEligibleForBundlingColumns) {
      styles.add(style8);
    }

    return styles;
  }

  private static List<CellStyle> getThirdHeaderStyles(XSSFWorkbook workbook, int headerStyles,
    MerchantType merchantType, boolean isEligibleForBundlingColumns, boolean isOtherAttributes,  boolean isRecommendedAttributes) {
    Font font = workbook.createFont();
    font.setBold(true);
    font.setColor(HSSFColor.WHITE.index);

    // Cell style
    XSSFCellStyle style1 = workbook.createCellStyle();
    style1.setFillForegroundColor(new XSSFColor(new java.awt.Color(169, 208, 142)));
    commonCellStyle(font, style1);

    XSSFCellStyle style2 = workbook.createCellStyle();
    style2.setFillForegroundColor(new XSSFColor(new java.awt.Color(255, 217, 102)));
    commonCellStyle(font, style2);

    XSSFCellStyle style3 = workbook.createCellStyle();
    style3.setFillForegroundColor(new XSSFColor(new java.awt.Color(155, 194, 230)));
    commonCellStyle(font, style3);

    XSSFCellStyle style4 = workbook.createCellStyle();
    style4.setFillForegroundColor(new XSSFColor(new java.awt.Color(244, 176, 132)));
    commonCellStyle(font, style4);

    XSSFCellStyle style5 = workbook.createCellStyle();
    style5.setFillForegroundColor(new XSSFColor(new java.awt.Color(142, 169, 219)));
    commonCellStyle(font, style5);

    XSSFCellStyle style8 = workbook.createCellStyle();
    style8.setFillForegroundColor(new XSSFColor(
      new java.awt.Color(RGB_RED_VALUE_BFB_HEADER_2, RGB_GREEN_VALUE_BFB_HEADER_2,
        RGB_BLUE_VALUE_BFB_HEADER_2)));
    commonCellStyle(font, style8);

    List<CellStyle> styles = new ArrayList<CellStyle>();
    styles.add(style1);
    styles.add(style2);
    styles.add(style3);
    styles.add(style4);
    styles.add(style5);
    if (merchantType.getType() > BFF_SELLER_TYPE_VALUE) {
      styles.add(style8);
    }
    styles.add(style1);

    if (isRecommendedAttributes) {
      XSSFCellStyle style6 = workbook.createCellStyle();
      style6.setFillForegroundColor(new XSSFColor(new java.awt.Color(255, 192, 0)));
      commonCellStyle(font, style6);
      styles.add(style6);
    }

    if (isOtherAttributes) {
      XSSFCellStyle style7 = workbook.createCellStyle();
      style7.setFillForegroundColor(new XSSFColor(new java.awt.Color(201, 201, 201)));
      commonCellStyle(font, style7);
      styles.add(style7);
    }

    if (isEligibleForBundlingColumns) {
      styles.add(style8);
    }

    return styles;
  }

  private static List<CellStyle> getFourthHeaderStyles(XSSFWorkbook workbook, int headerStyles,
    MerchantType merchantType, boolean isEligibleForBundlingColumns, boolean isOtherAttributes, boolean isRecommendedAttributes) {
    Font font = workbook.createFont();
    font.setBold(false);
    // Cell style
    XSSFCellStyle style1 = workbook.createCellStyle();
    style1.setFillForegroundColor(new XSSFColor(new java.awt.Color(226, 239, 218)));
    commonCellStyle(font, style1);
    style1.setWrapText(true);

    XSSFCellStyle style2 = workbook.createCellStyle();
    style2.setFillForegroundColor(new XSSFColor(new java.awt.Color(255, 242, 204)));
    commonCellStyle(font, style2);
    style2.setWrapText(true);

    XSSFCellStyle style3 = workbook.createCellStyle();
    style3.setFillForegroundColor(new XSSFColor(new java.awt.Color(221, 235, 247)));
    commonCellStyle(font, style3);
    style3.setWrapText(true);
    style3.setAlignment(HorizontalAlignment.LEFT);

    XSSFCellStyle style4 = workbook.createCellStyle();
    style4.setFillForegroundColor(new XSSFColor(new java.awt.Color(252, 228, 214)));
    commonCellStyle(font, style4);
    style4.setWrapText(true);

    XSSFCellStyle style5 = workbook.createCellStyle();
    style5.setFillForegroundColor(new XSSFColor(new java.awt.Color(217, 225, 242)));
    commonCellStyle(font, style5);
    style5.setWrapText(true);

    List<CellStyle> styles = new ArrayList<CellStyle>();
    styles.add(style1);
    styles.add(style2);
    styles.add(style3);
    styles.add(style4);
    styles.add(style5);
    if (merchantType.getType() > BFF_SELLER_TYPE_VALUE) {
      styles.add(style4);
    }

    styles.add(style1);

    if (isRecommendedAttributes) {
      XSSFCellStyle style6 = workbook.createCellStyle();
      style6.setFillForegroundColor(new XSSFColor(new java.awt.Color(255, 242, 204)));
      commonCellStyle(font, style6);
      style6.setWrapText(true);
      styles.add(style6);
    }

    if (isOtherAttributes) {
      XSSFCellStyle style7 = workbook.createCellStyle();
      style7.setFillForegroundColor(new XSSFColor(new java.awt.Color(237, 237, 237)));
      commonCellStyle(font, style7);
      style7.setWrapText(true);
      styles.add(style7);
    }


    if (isEligibleForBundlingColumns) {
      styles.add(style4);
    }

    return styles;
  }

  private static void commonCellStyle(Font font, XSSFCellStyle style) {
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    style.setAlignment(HorizontalAlignment.CENTER);
    style.setVerticalAlignment(VerticalAlignment.CENTER);
    style.setFont(font);
    style.setLeftBorderColor(IndexedColors.WHITE.getIndex());
    style.setRightBorderColor(IndexedColors.WHITE.getIndex());
    style.setTopBorderColor(IndexedColors.WHITE.getIndex());
    style.setBottomBorderColor(IndexedColors.WHITE.getIndex());
    style.setBorderTop(BorderStyle.THIN);
    style.setBorderBottom(BorderStyle.THIN);
    style.setBorderRight(BorderStyle.THIN);
    style.setBorderLeft(BorderStyle.THIN);
  }

  private static void commonCellStyleFirstHeader(XSSFWorkbook workbook, XSSFCellStyle style, short index) {
    Font font = workbook.createFont();
    font.setBold(true);
    font.setColor(index);
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    style.setAlignment(HorizontalAlignment.CENTER);
    style.setVerticalAlignment(VerticalAlignment.CENTER);
    style.setFont(font);
    style.setLeftBorderColor(IndexedColors.WHITE.getIndex());
    style.setRightBorderColor(IndexedColors.WHITE.getIndex());
    style.setTopBorderColor(IndexedColors.WHITE.getIndex());
    style.setBottomBorderColor(IndexedColors.WHITE.getIndex());
    style.setBorderTop(BorderStyle.THIN);
    style.setBorderBottom(BorderStyle.THIN);
    style.setBorderRight(BorderStyle.THIN);
    style.setBorderLeft(BorderStyle.THIN);
  }

  public static String updateSubjectToVatTemplate(MultipartFile request, String vatFilePath,
    String bulkProcessCode) throws Exception {
    String excelFileName;
    if (isFileXlsxType(request)) {
      excelFileName = request.getOriginalFilename();
    } else {
      log.error("Uploading template of invalid format: {}", request.getOriginalFilename());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessages.EXCEL_FILE_TYPE_INVALID_VAT_MESSAGE);
    }
    log.info("Uploading the file at controller. File name : {}", excelFileName);
    excelFileName = vatFilePath + Constants.ROOT + bulkProcessCode + Constants.ROOT;
    File file = new File(excelFileName);
    if (!file.exists()) {
      file.mkdirs();
    }
    excelFileName = excelFileName + bulkProcessCode + XLSX;
    file = new File(excelFileName);
    log.info("Transferring the file to destination : {}", file.getAbsolutePath());
    request.transferTo(file);
    return excelFileName;
  }

  public static boolean isFileXlsxType(MultipartFile request) {
    return request.getOriginalFilename().substring(request.getOriginalFilename().lastIndexOf(DOT))
        .equals(XLSX);
  }

  public static boolean isFileExcelType(MultipartFile request) {
    String extension = request.getOriginalFilename()
        .substring(request.getOriginalFilename().lastIndexOf(DOT));
    return EXCEL_FILE_TYPES.contains(extension);
  }

  public static boolean isWhiteListedSeller(ProfileResponse profileResponse) {
    return profileResponse.getCompany().isCncActivated() || Boolean.TRUE
        .equals(profileResponse.getMultiDefaultAddressFlag());
  }

  public static XSSFWorkbook dropDownCreation(Blob blob, List<String> wareHouseCodesAndNames, String type)
      throws IOException {
    InputStream inputStream = new ByteArrayInputStream(blob.getContent());
    XSSFWorkbook workbook = new XSSFWorkbook(inputStream);
    XSSFSheet sheet = workbook.getSheetAt(DATA_SHEET_INDEX);
    setWareHouseSheet(wareHouseCodesAndNames, workbook);
    int wareHouseColumnIndex = StringUtils.equals(type, TRANSFER_REQUEST) ?
        WAREHOUSE_COLUMN_NO_FOR_TRANSFER_REQUEST :
        WAREHOUSE_COLUMN_FOR_ASSEMBLY_DISASSEMBLY;
    DataValidationHelper dvHelper = sheet.getDataValidationHelper();
    String wareHouseCodesAndNamesFormula =
        new StringBuilder(WAREHOUSE).append("!$A$2").append(":$A$").append(wareHouseCodesAndNames.size() + 1)
            .toString();
    DataValidationConstraint dvConstraint = dvHelper.createFormulaListConstraint(wareHouseCodesAndNamesFormula);
    CellRangeAddressList addressList =
        new CellRangeAddressList(FIRST_ROW_ASSEMBLY, LAST_ROW, wareHouseColumnIndex, wareHouseColumnIndex);
    DataValidation validation = dvHelper.createValidation(dvConstraint, addressList);
    if (StringUtils.equals(type, ASSEMBLY_REQUEST)) {
      CellRangeAddressList addressListForBundlingType =
          new CellRangeAddressList(FIRST_ROW_ASSEMBLY, LAST_ROW, BUNDLING_TYPE_COLUMN, BUNDLING_TYPE_COLUMN);
      DataValidationConstraint dvConstraintForBundlingType = dvHelper.createExplicitListConstraint(BUNDLING_TYPE);
      DataValidation validationForBundlingType =
          dvHelper.createValidation(dvConstraintForBundlingType, addressListForBundlingType);
      sheet.addValidationData(validationForBundlingType);
    }
    sheet.addValidationData(validation);
    return workbook;
  }

  private static void setWareHouseSheet(List<String> wareHouseCodesAndNames, XSSFWorkbook workbook) {
    XSSFSheet wareHouseSheet = workbook.getSheetAt(WAREHOUSE_SHEET_INDEX);
    int startRow = 1;
    for (String wareHouseCodesAndName : wareHouseCodesAndNames) {
      Row row = wareHouseSheet.createRow(startRow);
      Cell cell = row.createCell(WAREHOUSE_CODE_NAME_COLUMN);
      cell.setCellValue(wareHouseCodesAndName);
      startRow++;
    }
  }

  private static boolean isPreOrderActive(PreOrderDTO preOrderDTO) {
    boolean isPreOrder = Optional.ofNullable(preOrderDTO.getIsPreOrder()).orElse(false);
    Date preOrderDate = preOrderDTO.getPreOrderDate();
    return isPreOrder && Objects.nonNull(preOrderDate) && new Date().before(preOrderDate);
  }
}
