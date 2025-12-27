package com.gdn.mta.bulk.helper;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.PickupPointModel;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.google.common.collect.ImmutableList;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormat;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFColor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;

/**
 * Created by keshashah on 25/10/16.
 */
@Component(value = "bulkProductProcessHelper")
@Slf4j
public class BulkProductProcessHelper extends BulkProcessHelper {
  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.BLIBLI_PRODUCT_SKU)
          .add(BulkParameters.PARENT_PRODUCT_NAME).add(BulkParameters.BLIBLI_SKU)
          .add(BulkParameters.PRODUCT_NAME).build();

  private static final byte[] LIGHT_GREEN_RGB = {(byte) 226, (byte) 239, (byte) 218};
  private static final byte[] GREEN_RGB = {(byte) 169, (byte) 208, (byte) 142};
  private static final byte[] PEACH_RGB = {(byte) 248, (byte) 204, (byte) 173};
  private static final byte[] LIGHT_PEACH_RGB = {(byte) 252, (byte) 228, (byte) 214};
  private static final int FOURTH_ROW_INDEX = 3;
  private static final int FIRST_ROW_INDEX = 0;
  private static final int FOURTH_ROW_HEIGHT = 96;
  private static final int FIRST_ROW_HEIGHT = 40;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${pre.order.quota.feature.switch}")
  private boolean preOrderQuotaFeatureSwitch;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    BulkProductResponse productResponse = (BulkProductResponse) bulkDataResponse;
    boolean isBusinessPartnerO2O = productResponse.isBusinessPartnerO2O();
    ProfileResponse profileResponse = businessPartnerRepository
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkDataResponse.getBusinessPartnerCode());
    boolean isOnlyExternalUser =
        productResponse.getPrivilegedMap().getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    boolean isBusinessPartnerB2b = isMerchantB2b(profileResponse);
    List<String> primaryHeaders = new ArrayList<>(HEADER_LIST);
    List<String> secondaryHeaders = new ArrayList<>(
        Arrays.asList(BulkParameters.MANDATORY, BulkParameters.OPTIONAL, BulkParameters.MANDATORY,
            BulkParameters.OPTIONAL));
    List<String> quaternaryHeaders = new ArrayList<>(
        Arrays.asList(BulkParameters.BLIBLI_PRODUCT_SKU_DESC,
            BulkParameters.PARENT_PRODUCT_NAME_DESC, BulkParameters.BLIBLI_SKU_DESC,
            BulkParameters.PRODUCT_NAME_DESC));
    List<String> tertiaryHeaders = new ArrayList<>(
        Arrays.asList(BulkParameters.NOT_EDITABLE, BulkParameters.NOT_EDITABLE,
            BulkParameters.NOT_EDITABLE, BulkParameters.NOT_EDITABLE));

    if (Boolean.FALSE
        .equals(productResponse.getPrivilegedMap().get(BulkParameters.IS_ONLY_EXTERNAL_USER))) {
      primaryHeaders.add(BulkParameters.SKU_CODE_HEADER);
      secondaryHeaders.add(BulkParameters.OPTIONAL);
      tertiaryHeaders.add(BulkParameters.NOT_EDITABLE);
      quaternaryHeaders.add(BulkParameters.SKU_CODE_HEADER_DESC);
    }
    primaryHeaders.add(BulkParameters.SELLER_SKU);
    secondaryHeaders.add(BulkParameters.OPTIONAL);
    tertiaryHeaders.add(BulkParameters.EDITABLE);
    quaternaryHeaders.add(BulkParameters.SELLER_SKU_DESC);
    Map<String, Boolean> privilegedMap = productResponse.getPrivilegedMap();
    if (privilegedMap.containsKey(BulkParameters.PRIVILEGE_READ_PRICE)) {
      primaryHeaders.add(BulkParameters.PRICE_HEADER);
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(BulkParameters.PRICE_HEADER_DESC);

      primaryHeaders.add(BulkParameters.SELLING_PRICE_HEADER);
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(BulkParameters.SELLING_PRICE_DESC);
    }
    checkForStockAndProductType(privilegedMap, primaryHeaders, secondaryHeaders, tertiaryHeaders,
        quaternaryHeaders, CommonUtils.isOMGSeller(profileResponse));
    if (privilegedMap.containsKey(BulkParameters.PRIVILEGE_READ_PICKUP_POINT)) {
      primaryHeaders.add(3, BulkParameters.PICKUP_POINT_HEADER);
      secondaryHeaders.add(3,BulkParameters.MANDATORY);
      tertiaryHeaders.add(3,BulkParameters.EDITABLE);
      quaternaryHeaders.add(3,BulkParameters.PICKUP_POINT_HEADER_DESC);

      primaryHeaders.add(4,BulkParameters.PICKUP_POINT_NAME_HEADER);
      secondaryHeaders.add(4,BulkParameters.OPTIONAL);
      tertiaryHeaders.add(4,BulkParameters.NOT_EDITABLE);
      quaternaryHeaders.add(4,BulkParameters.PICKUP_POINT_NAME_HEADER_DESC);
    }
    checkForProductFlags(privilegedMap, isBusinessPartnerO2O, primaryHeaders, secondaryHeaders,
        tertiaryHeaders, quaternaryHeaders, profileResponse, multiPickupPointEnabled,
        cncForWarehouseFeatureSwitch);
    checkForWarehouseStockFlag(privilegedMap, primaryHeaders, secondaryHeaders, tertiaryHeaders,
        quaternaryHeaders);
    checkForB2bFlag(isOnlyExternalUser, isBusinessPartnerB2b, primaryHeaders, secondaryHeaders,
        tertiaryHeaders, quaternaryHeaders);
    return getAllHeaderList(primaryHeaders, secondaryHeaders, tertiaryHeaders, quaternaryHeaders);
  }


  private List<String> getAllHeaderList(List<String> primaryHeaders, List<String> secondaryHeader,
      List<String> tertiaryHeaders, List<String> quaternaryHeaders) throws JsonProcessingException {
    List<String> headerList = new ArrayList<>();
    try {
      // Going forward we will have 4 rows as headers, so we will serialise it here as string and
      // again deserialize it when needed
      headerList.add(objectMapper.writeValueAsString(primaryHeaders));
      headerList.add(objectMapper.writeValueAsString(secondaryHeader));
      headerList.add(objectMapper.writeValueAsString(tertiaryHeaders));
      headerList.add(objectMapper.writeValueAsString(quaternaryHeaders));
    } catch (Exception exception) {
      log.error("Error while writing headers ", exception);
      headerList = new ArrayList<>();
    }
    return headerList;
  }

  private static void checkForB2bFlag(boolean isOnlyExternalUser, boolean isBusinessPartnerB2b,
      List<String> primaryHeaders, List<String> secondaryHeaders, List<String> tertiaryHeaders, List<String> quaternaryHeaders) {
    if (isBusinessPartnerB2b) {
      primaryHeaders.add(BulkParameters.BFB_BASE_PRICE);
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(BulkParameters.BFB_BASE_PRICE_DESC);

      primaryHeaders.add(BulkParameters.BFB_MANAGED);
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(BulkParameters.BFB_STATUS_AND_MANAGED_HEADER_DESC);

      primaryHeaders.add(isOnlyExternalUser ?
          BulkParameters.EXTERNAL_BFB_STATUS :
          BulkParameters.AMPHI_BFB_STATUS);
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(isOnlyExternalUser ?
          BulkParameters.BFB_STATUS_AND_MANAGED_HEADER_DESC :
          BulkParameters.AMPHI_BFB_STATUS_AND_MANAGED_HEADER_DESC);
    }
  }

  private boolean isMerchantB2b(ProfileResponse profileResponse) {
    MerchantStatusType merchantStatusType = BulkCreationCommonUtil.getMerchantType(profileResponse);
    return merchantStatusType.getType() >= MerchantStatusType.BFB.getType();
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    BulkProductResponse productResponse = (BulkProductResponse) response;
    Sheet pickupPointSheet = workbook.createSheet(BulkParameters.PICKUP_POINT_SHEET);
    int rowindex = 0;
    Row row;
    response.setBusinessPartnerCode(response.getBusinessPartnerCode());
    ProfileResponse profileResponse = businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, response.getBusinessPartnerCode());
    boolean whiteListedSeller =
        Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getMultiDefaultAddressFlag()) ?
            profileResponse.getMultiDefaultAddressFlag() :
            false;
    for (PickupPointModel pickupPoint : productResponse.getPickupPoint()) {
      row = pickupPointSheet.createRow((short) rowindex);
      rowindex++;
      Cell cell = row.createCell((short) 0);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getCode());
      cell = row.createCell((short) 1);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getName());
      if(whiteListedSeller && pickupPoint.isFbbActivated()) {
        cell = row.createCell((short) 2);
        cell.setCellType(Cell.CELL_TYPE_STRING);
        cell.setCellValue(Constant.FBB);
      }
    }

    int pickupPointIndex = BulkParameters.PICKUP_POINT_DEFAULT_COLUMN_INDEX;
    Map<String, Boolean> privilegedMap = productResponse.getPrivilegedMap();
    bulkDownloadServiceBeanUtil.generateValidationForWorkbook(workbook,
      productResponse.getPickupPoint().size(), pickupPointIndex,
      privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, false),
      BulkParameters.PICKUP_POINT_SHEET);

    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkProductResponse productResponse = (BulkProductResponse) response;
    return productResponse.getProductContentList();
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT) + request.getRequestId();
  }

  public String getFailedProductDirectory(String bulkProcessCode) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.FAILED_PRODUCT) + bulkProcessCode;
  }


  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put("name", getUserName(request.getUsername()));
    emailParameters.put("reqId", request.getRequestId());
    emailParameters.put("businessPartnerCode", request.getMerchantId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, MessageUtil.getMessage(EmailConstants.PRODUCT_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM,EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, MessageUtil.getMessage(EmailConstants.PRODUCT_MAIL_SUBJECT, lang));
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkProductResponse productResponse = (BulkProductResponse) response;
    return productResponse.getProductContentList().size();
  }

  private void checkForStockAndProductType(Map<String, Boolean> privilegedMap,
      List<String> primaryHeaders, List<String> secondaryHeaders, List<String> tertiaryHeaders,
      List<String> quaternaryHeaders, boolean isOmgSeller) {
    if (privilegedMap.containsKey(BulkParameters.PRIVILEGE_READ_STOCK)) {
      primaryHeaders.add(BulkParameters.STOCK_HEADER);
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(BulkParameters.STOCK_DESC);

      if (isOmgSeller && preOrderQuotaFeatureSwitch) {
        primaryHeaders.add(BulkParameters.PO_QUOTA);
        secondaryHeaders.add(BulkParameters.OPTIONAL);
        tertiaryHeaders.add(BulkParameters.EDITABLE);
        quaternaryHeaders.add(BulkParameters.PO_QUOTA_DESCRIPTION);
      }

      primaryHeaders.add(BulkParameters.STOCK_REMINDER_ID);
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(BulkParameters.STOCK_REMINDER_DESC);
    }
    if (privilegedMap.containsKey(BulkParameters.PRIVILEGE_READ_PRODUCT_TYPE)) {
      primaryHeaders.add(BulkParameters.TYPE_HANDLING_HEADER);
      secondaryHeaders.add(Constant.HYPHEN);
      tertiaryHeaders.add(Constant.HYPHEN);
      quaternaryHeaders.add(Constant.HYPHEN);
    }
  }

  private static void checkForProductFlags(Map<String, Boolean> privilegedMap,
      boolean isBusinessPartnerO2O, List<String> primaryHeaders, List<String> secondaryHeaders,
      List<String> tertiaryHeaders, List<String> quaternaryHeaders, ProfileResponse profileResponse,
      boolean multiPickupPointEnabled, boolean cncForWarehouseFeatureSwitch) {

    if (cncForWarehouseFeatureSwitch) {
      if (privilegedMap.containsKey(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE)) {
        if (!privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)) {
          primaryHeaders.add(BulkParameters.AMPHI_SKU_STATUS_NEW);
          secondaryHeaders.add(BulkParameters.MANDATORY);
          tertiaryHeaders.add(BulkParameters.EDITABLE);
          quaternaryHeaders.add(BulkParameters.AMPHI_STATUS_HEADER_DESC);
        } else {
          primaryHeaders.add(BulkParameters.EXTERNAL_SKU_STATUS_NEW);
          secondaryHeaders.add(BulkParameters.MANDATORY);
          tertiaryHeaders.add(BulkParameters.EDITABLE);
          quaternaryHeaders.add(BulkParameters.PURE_EXTERNAL_STATUS_HEADER_DESC);
        }
        if (profileResponse.getCompany().isCncActivated()) {
          primaryHeaders.add(BulkParameters.DELIVERY_STATUS_HEADER);
          secondaryHeaders.add(BulkParameters.MANDATORY);
          tertiaryHeaders.add(BulkParameters.EDITABLE);
          quaternaryHeaders.add(BulkParameters.DELIVERY_STATUS_HEADER_DESC);
        }
      }
    } else {
      if (privilegedMap.containsKey(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE)) {
        if (!privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)) {
          primaryHeaders.add(BulkParameters.AMPHI_SKU_STATUS);
          secondaryHeaders.add(BulkParameters.MANDATORY);
          tertiaryHeaders.add(BulkParameters.EDITABLE);
          quaternaryHeaders.add(BulkParameters.AMPHI_STATUS_HEADER_DESC);
        } else {
          primaryHeaders.add(BulkParameters.EXTERNAL_SKU_STATUS);
          secondaryHeaders.add(BulkParameters.MANDATORY);
          tertiaryHeaders.add(BulkParameters.EDITABLE);
          quaternaryHeaders.add(BulkParameters.PURE_EXTERNAL_STATUS_HEADER_DESC);
        }
      }
    }
    if (multiPickupPointEnabled && profileResponse.getCompany().isCncActivated()) {
      if (!cncForWarehouseFeatureSwitch) {
        primaryHeaders.add(BulkParameters.EXTERNAL_CNC_STATUS_HEADER);
      } else {
        primaryHeaders.add(BulkParameters.CNC_STATUS_HEADER);
      }
      secondaryHeaders.add(BulkParameters.MANDATORY);
      tertiaryHeaders.add(BulkParameters.EDITABLE);
      quaternaryHeaders.add(BulkParameters.CNC_HEADER_DESC);
    }

    if (isBusinessPartnerO2O && privilegedMap.containsKey(BulkParameters.PRIVILEGE_READ_O2O)) {
      primaryHeaders.add(BulkParameters.IN_STORE_HEADER);
      secondaryHeaders.add(BulkParameters.OPTIONAL);
      tertiaryHeaders.add(BulkParameters.NOT_EDITABLE);
      quaternaryHeaders.add(BulkParameters.IN_STORE_HEADER_DESC);
    }
  }

  private static void checkForWarehouseStockFlag(Map<String, Boolean> privilegedMap,
      List<String> primaryHeaders, List<String> secondaryHeaders, List<String> tertiaryHeaders,
      List<String> quaternaryHeaders) {
    Boolean privilegedReadWarehouseStock = privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, false);
    if (privilegedReadWarehouseStock != null && privilegedReadWarehouseStock) {
      primaryHeaders.add(BulkParameters.WAREHOUSE_STOCK_HEADER);
      secondaryHeaders.add(BulkParameters.OPTIONAL);
      tertiaryHeaders.add(BulkParameters.NOT_EDITABLE);
      quaternaryHeaders.add(BulkParameters.WAREHOUSE_STOCK_HEADER_DESC);
    }
  }

  public Workbook generateDataSheet(List<String> headers, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) throws IOException {
    Workbook workbook = new SXSSFWorkbook();
    List<List<String>> headerList = getDividedHeadersList(headers);
    return addSheetsAndGetWorkbook(headerList, rowDataList, workbook, BulkParameters.DATA_SHEET,
        bulkUpdateTemplateColumnWidth);
  }

  private Workbook addSheetsAndGetWorkbook(List<List<String>> headerList, List<List<String>> rowDataList,
      Workbook workbook, String sheetName, int bulkUpdateTemplateColumnWidth) {
    Sheet dataSheet = workbook.createSheet(sheetName);
    DataFormat format = workbook.createDataFormat();
    CellStyle styleForSellerSKU = workbook.createCellStyle();

    List<String> primaryHeaders = headerList.get(0);
    List<String> secondaryHeaders = headerList.get(1);
    List<String> tertiaryHeaders = headerList.get(2);
    List<String> quaternaryHeaders = headerList.get(3);
    Row row;
    int rowId = 0;
    int sellerSkuColumnId = primaryHeaders.indexOf(BulkParameters.SELLER_SKU);
    createHeaderRow(dataSheet, workbook, rowId++, primaryHeaders, tertiaryHeaders, false, false,
        true);
    createHeaderRow(dataSheet, workbook, rowId++, secondaryHeaders, tertiaryHeaders, false, false,
        false);
    createHeaderRow(dataSheet, workbook, rowId++, tertiaryHeaders, tertiaryHeaders, false, false,
        false);
    createHeaderRow(dataSheet, workbook, rowId++, quaternaryHeaders, tertiaryHeaders, true, true,
        false);
    int columnWidth = bulkUpdateTemplateColumnWidth * 256;
    IntStream.range(0, primaryHeaders.size())
        .forEach(col -> dataSheet.setColumnWidth(col, columnWidth));
    dataSheet.getRow(FOURTH_ROW_INDEX).setHeightInPoints(FOURTH_ROW_HEIGHT);
    dataSheet.getRow(FIRST_ROW_INDEX).setHeightInPoints(FIRST_ROW_HEIGHT);
    for (List<String> rowData : rowDataList) {
      row = dataSheet.createRow(rowId);
      rowId++;
      int cellId = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellId);
        if (sellerSkuColumnId == cellId) {
          styleForSellerSKU.setDataFormat(format.getFormat(Constant.AT_SIGN));
          cell.setCellStyle(styleForSellerSKU);
        }
        cell.setCellValue(Optional.ofNullable(cellValue).orElse(StringUtils.EMPTY));
        cellId++;
      }
    }
    return workbook;
  }

  private static void createHeaderRow(Sheet dataSheet, Workbook workbook, int rowIndex, List<String> headers,
      List<String> tertiaryHeaders, boolean quaternaryHeader, boolean applyTopBottomBorder, boolean primaryHeader) {
    Row row = dataSheet.createRow(rowIndex);
    int cellIndex = 0;
    int tertiaryHeaderIterator = 0;

    for (String header : headers) {
      Cell cell = row.createCell(cellIndex++);
      cell.setCellType(CellType.STRING);
      CellStyle cellStyle = createCellStyle(workbook, header, tertiaryHeaders, tertiaryHeaderIterator, quaternaryHeader,
          applyTopBottomBorder, primaryHeader);
      cellStyle.setWrapText(true);
      cell.setCellStyle(cellStyle);
      cell.setCellValue(header);
      tertiaryHeaderIterator++;
    }
  }

  private static CellStyle createCellStyle(Workbook workbook, String header, List<String> tertiaryHeaders,
      int tertiaryHeaderIterator, boolean quaternaryHeader, boolean applyTopBottomBorder, boolean primaryHeader) {
    CellStyle cellStyle = workbook.createCellStyle();
    cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    byte[] rgb = getColorForHeader(tertiaryHeaders, tertiaryHeaderIterator, quaternaryHeader);
      XSSFCellStyle xssfCellStyle = (XSSFCellStyle) cellStyle;
      XSSFColor customColor = new XSSFColor(rgb, null);
      xssfCellStyle.setFillForegroundColor(customColor);
    setBorders(cellStyle, applyTopBottomBorder);
    setFontStyle(workbook, cellStyle, header, primaryHeader, quaternaryHeader);
    setAlignment(cellStyle, primaryHeader);
    return cellStyle;
  }

  private static byte[] getColorForHeader(List<String> tertiaryHeaders, int tertiaryHeaderIterator,
      boolean quaternaryHeader) {
    if (BulkParameters.EDITABLE.equals(tertiaryHeaders.get(tertiaryHeaderIterator))) {
      return quaternaryHeader ? LIGHT_GREEN_RGB : GREEN_RGB;
    }
    return quaternaryHeader ? LIGHT_PEACH_RGB : PEACH_RGB;
  }

  private static void setBorders(CellStyle cellStyle, boolean applyTopBottomBorder) {
    cellStyle.setBorderLeft(BorderStyle.THIN);
    cellStyle.setBorderRight(BorderStyle.THIN);
    cellStyle.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setRightBorderColor(IndexedColors.BLACK.getIndex());
    if (applyTopBottomBorder) {
      cellStyle.setBorderTop(BorderStyle.THIN);
      cellStyle.setTopBorderColor(IndexedColors.BLACK.getIndex());
      cellStyle.setBorderBottom(BorderStyle.THIN);
      cellStyle.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    }
  }

  private static void setFontStyle(Workbook workbook, CellStyle cellStyle, String header, boolean primaryHeader,
      boolean quaternaryHeader) {
    Font boldFont = workbook.createFont();
    boldFont.setBold(!quaternaryHeader);
    boldFont.setFontHeightInPoints((short) (primaryHeader ? 14 : 12));
    if (BulkParameters.MANDATORY.equals(header)) {
      boldFont.setColor(IndexedColors.RED.getIndex());
    }
    cellStyle.setFont(boldFont);
  }

  private static void setAlignment(CellStyle cellStyle, boolean primaryHeader) {
    cellStyle.setWrapText(true);
    cellStyle.setAlignment(HorizontalAlignment.CENTER);
    cellStyle.setVerticalAlignment(VerticalAlignment.CENTER);
    if (primaryHeader) {
      cellStyle.setIndention((short) 4);
    }
  }

  private List<List<String>> getDividedHeadersList(List<String> headerList) throws IOException {
    String primaryHeader = headerList.get(0);
    String secondaryHeader = headerList.get(1);
    String tertiaryHeaders = headerList.get(2);
    String quaternaryHeaders = headerList.get(3);
    List<String> primaryHeaderList =
        objectMapper.readValue(primaryHeader, new TypeReference<>() {});
    List<String> secondaryHeaderList =
        objectMapper.readValue(secondaryHeader, new TypeReference<>() {});
    List<String> tertiaryHeaderList =
        objectMapper.readValue(tertiaryHeaders, new TypeReference<>() {});
    List<String> quartenaryHeaderList =
        objectMapper.readValue(quaternaryHeaders, new TypeReference<>() {});
    return Arrays.asList(primaryHeaderList, secondaryHeaderList, tertiaryHeaderList,
        quartenaryHeaderList);
  }

  public Workbook callSuperGenerateDataSheet(List<String> headerListWithError, List<List<String>> xlData,
      int bulkUpdateTemplateColumnWidth) throws IOException {
    return super.generateDataSheet(headerListWithError, xlData, bulkUpdateTemplateColumnWidth);
  }

}
