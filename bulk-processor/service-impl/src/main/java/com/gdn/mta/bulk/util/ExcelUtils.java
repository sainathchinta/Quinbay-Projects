package com.gdn.mta.bulk.util;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.IntStream;

import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.dto.QrCodeErrorDTO;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.service.BulkGenericProcessorServiceBean;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.google.common.collect.ImmutableMap;

public class ExcelUtils {

  public static boolean isProductSkuPresent = false;
  public static boolean isItemSkuPresent = false;
  public static boolean isPickupPointNamePresent = false;
  private static final Map<Boolean, String> CHILD_SKU =
      ImmutableMap.of(true, GenericBulkHeaders.CHILD_SKU_EN, false, GenericBulkHeaders.CHILD_SKU_ID);
  private static final Map<Boolean, String> QUANTITY =
      ImmutableMap.of(true, GenericBulkHeaders.QUANTITY_EN, false, GenericBulkHeaders.QUANTITY_ID);

  private static final Map<Boolean, String> INSTORE_DESC_MAP =
      ImmutableMap.of(true, BulkCnCreationHeaderNames.INSTORE_DESC_EN, false,
          BulkCnCreationHeaderNames.INSTORE_DESC_ID);

  public static List<List<Object>> read(FileInputStream fileInputStream, Integer sheetIndex, Integer rowStart)
      throws Exception {
    HSSFWorkbook workbook = new HSSFWorkbook(fileInputStream);
    HSSFSheet sheet = workbook.getSheetAt(sheetIndex);
    return getDataRows(rowStart, workbook, sheet, Collections.emptyList());
  }

  public static String getCategoryCodeFromExcel(InputStream fileInputStream, String excelFileType)
      throws Exception {
    if (Constant.FILE_TYPE_XLS.equalsIgnoreCase(excelFileType)) {
      HSSFWorkbook workbook = new HSSFWorkbook(fileInputStream);
      HSSFSheet sheet = workbook.getSheet(BulkParameters.CATEGORY_SHEET);
      if (Objects.nonNull(sheet)) {
        if (sheet.getLastRowNum() == 0 && Objects.isNull(sheet.getRow(0))) {
          return StringUtils.EMPTY;
        } else {
          return sheet.getRow(0).getCell(0).getStringCellValue();
        }
      }
    } else {
      XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
      XSSFSheet sheet = workbook.getSheet(BulkParameters.CATEGORY_SHEET);
      if (Objects.nonNull(sheet)) {
        if (sheet.getLastRowNum() == 0 && Objects.isNull(sheet.getRow(0))) {
          return StringUtils.EMPTY;
        } else {
          return sheet.getRow(0).getCell(0).getStringCellValue();
        }
      }
    }
    return StringUtils.EMPTY;
  }

  public static List<List<Object>> readByDataSheetName(ByteArrayInputStream fileInputStream, String sheetName,
      Integer rowStart, List<Integer> excludeColumns, String excelFileType) throws Exception {
    if (Constant.FILE_TYPE_XLS.equalsIgnoreCase(excelFileType)) {
      HSSFWorkbook workbook = new HSSFWorkbook(fileInputStream);
      HSSFSheet sheet = workbook.getSheet(sheetName);
      return getDataRows(rowStart, workbook, sheet, excludeColumns);
    } else {
      XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
      XSSFSheet sheet = workbook.getSheet(sheetName);
      return getDataRows(rowStart, workbook, sheet, excludeColumns);
    }
  }


  private static List<List<Object>> getDataRows(Integer rowStart, Workbook workbook, Sheet sheet,
      List<Integer> excludeColumns) throws IOException {
    Iterator<Row> rows = sheet.rowIterator();
    Integer columns = sheet.getRow(1).getPhysicalNumberOfCells();
    int i = 0;
    List<List<Object>> raws = new ArrayList<>();
    while (rows.hasNext()) {
      Row row = rows.next();
      if (i >= rowStart) {
        List<Object> raw = new ArrayList<>();
        int emptyStringCount = 0;
        for (int j = 0; j < columns; j++) {
          if (!excludeColumns.contains(j)) {
            Cell cell = row.getCell(j, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
            cell.setCellType(Cell.CELL_TYPE_STRING);
            if (StringUtils.isEmpty(cell.getStringCellValue().trim())) {
              emptyStringCount++;
            }
            raw.add(cell.getStringCellValue().trim());
          }
        }
        if (emptyStringCount != columns && !raw.isEmpty()) {
          raws.add(raw);
        }
      }
      i++;
    }
    workbook.close();
    return raws;
  }

  public static List<List<Object>> readByDataSheetName(ByteArrayInputStream fileInputStream, String sheetName,
      Integer rowStart, Integer rowEnd, String excelFileType, String bulkProcessCode, int maxRowCount, boolean validateMaxRowCount) throws Exception {
    if (Constant.FILE_TYPE_XLS.equalsIgnoreCase(excelFileType)) {
      HSSFWorkbook workbook = new HSSFWorkbook(fileInputStream);
      HSSFSheet sheet = workbook.getSheet(sheetName);
      POIUtil.validateNumberOfRows(sheet, bulkProcessCode, maxRowCount, validateMaxRowCount);
      return getDataRows(rowStart, rowEnd, workbook, sheet);
    } else {
      XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
      XSSFSheet sheet = workbook.getSheet(sheetName);
      POIUtil.validateNumberOfRows(sheet, bulkProcessCode, maxRowCount, validateMaxRowCount);
      return getDataRows(rowStart, rowEnd, workbook, sheet);
    }
  }


  private static List<List<Object>> getDataRows(Integer rowStart, Integer rowEnd, Workbook workbook, Sheet sheet)
      throws IOException {
    Iterator<Row> rows = sheet.rowIterator();
    Integer columns = sheet.getRow(1).getPhysicalNumberOfCells();
    int i = 0;
    List<List<Object>> raws = new ArrayList<>();
    while (rows.hasNext()) {
      Row row = rows.next();
      if (i >= rowStart && i < rowEnd) {
        List<Object> raw = new ArrayList<>();
        for (int j = 0; j < columns; j++) {
          Cell cell = row.getCell(j, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
          cell.setCellType(Cell.CELL_TYPE_STRING);
          raw.add(cell.getStringCellValue());
        }
        if (!raw.isEmpty()) {
          raws.add(raw);
        }
      }
      if (i >= rowEnd) {
        break;
      }
      i++;
    }
    workbook.close();
    return raws;
  }

  public static List<List<Object>> getExcelHeaders(boolean isInternationalMerchant,
      MerchantStatusType merchantStatusType, String merchantType, String productBundlingEligibleMerchantTypes,
      boolean productBundlingEnabled, boolean instoreSeller) {
    List<List<Object>> headerList = new ArrayList<>();
    switch (merchantStatusType) {
      case DELIVERY_AND_CNC:{
        if (isInternationalMerchant) {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_EN_MPP_SWITCH_ENABLED_CNC));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_EN_MPP_SWITCH_ENABLED_CNC));
        } else {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_MPP_SWITCH_ENABLED_CNC));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_MPP_SWITCH_ENABLED_CNC));
        }
        break;
      }
      case BFB:{
        if (isInternationalMerchant) {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_EN_NON_CNC_BFB));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_EN_NON_CNC_BFB));
        } else {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_NON_CNC_BFB));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_NON_CNC_BFB));
        }
        break;
      }
      case BFB_AND_CNC:{
        if (isInternationalMerchant) {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_EN_CNC_BFB));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_EN_CNC_BFB));
        } else {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_CNC_BFB));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_CNC_BFB));
        }
        break;
      }
      default:
        if (isInternationalMerchant) {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_EN_MPP_SWITCH_ENABLED_NON_CNC));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_EN_MPP_SWITCH_ENABLED_NON_CNC));
        } else {
          headerList.add(new ArrayList<>(GenericBulkHeaders.HEADER_LIST_MPP_SWITCH_ENABLED_NON_CNC));
          headerList.add(new ArrayList<>(GenericBulkHeaders.SECONDARY_HEADER_LIST_MPP_SWITCH_ENABLED_NON_CNC));
        }
        break;
    }
    if (ExcelTemplateUtil.isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes,
        productBundlingEnabled)) {
      headerList.stream().findFirst().ifPresent(columnList -> {
        columnList.add(CHILD_SKU.get(isInternationalMerchant));
        columnList.add(QUANTITY.get(isInternationalMerchant));
      });
      headerList.stream().skip(1).findFirst().ifPresent(columnList -> {
        IntStream.range(0, 2).forEach(i -> columnList.add(StringUtils.EMPTY));
      });
    }
    if (instoreSeller) {
      headerList.stream().findFirst().ifPresent(columnList -> columnList.add(
          BulkGenericProcessorServiceBean.SELLER_TYPE_INSTORE_COLUMN_INDEX_MAP.get(merchantStatusType.getType()),
          GenericBulkHeaders.INSTORE));
      headerList.stream().skip(1).findFirst().ifPresent(columnList -> columnList.add(
          BulkGenericProcessorServiceBean.SELLER_TYPE_INSTORE_COLUMN_INDEX_MAP.get(merchantStatusType.getType()),
          INSTORE_DESC_MAP.get(isInternationalMerchant)));
    }
    return headerList;
  }

  public static List<List<Object>> getCnExcelHeaders(boolean isInternationalMerchant, CategoryDetailResponse category,
      MerchantStatusType merchantStatusType, String merchantType, boolean productBundlingEnabled,
      String productBundlingEligibleMerchantTypes, boolean instoreSeller) {
    List<List<Object>> headerList = new ArrayList<>();
    List<List<String>> cnHeaderList;
    if (isInternationalMerchant) {
      cnHeaderList = CnBulkHeaders.cnHeaderListEn(category, merchantStatusType, merchantType, productBundlingEnabled,
          productBundlingEligibleMerchantTypes, instoreSeller);
    } else {
      cnHeaderList =
          CnBulkHeaders.cnHeaderList(category, merchantStatusType, merchantType, productBundlingEligibleMerchantTypes,
              productBundlingEnabled, instoreSeller);
    }
    headerList.add(new ArrayList<>(cnHeaderList.get(0)));
    headerList.add(new ArrayList<>(cnHeaderList.get(1)));
    return headerList;
  }

  public static void  generateErrorFileForQrGeneration(String filePath,
      List<QrCodeErrorDTO> errors) throws Exception {
    try (XSSFWorkbook xssfWorkbook = new XSSFWorkbook()) {
      XSSFSheet sheet = xssfWorkbook.createSheet();
      int rowNum = 0;
      int colNum = 0;
      Row headerRow = sheet.createRow(rowNum++);
      if(CollectionUtils.isNotEmpty(errors)) {
        populateHeaderFlags(errors.get(0).getQrGenerationType());
      }
      if (isProductSkuPresent) {
        headerRow.createCell(colNum++).setCellValue(ExcelHeaderNames.PRODUCT_SKU);
      }
      if (isItemSkuPresent) {
        headerRow.createCell(colNum++).setCellValue(ExcelHeaderNames.ITEM_SKU);
      }
      if (isPickupPointNamePresent) {
        headerRow.createCell(colNum++).setCellValue(ExcelHeaderNames.PICKUP_POINT_NAME);
      }
      headerRow.createCell(colNum).setCellValue(ExcelHeaderNames.ERROR_MESSAGE);
      for (QrCodeErrorDTO qrCodeErrorDTO : errors) {
        Row row = sheet.createRow(rowNum++);
        colNum = 0;
        if (isProductSkuPresent) {
          row.createCell(colNum++).setCellValue(qrCodeErrorDTO.getProductSku());
        }
        if (isItemSkuPresent) {
          row.createCell(colNum++).setCellValue(qrCodeErrorDTO.getItemSku());
        }
        if (isPickupPointNamePresent) {
          row.createCell(colNum++).setCellValue(qrCodeErrorDTO.getPickupPointName());
        }
        row.createCell(colNum).setCellValue(qrCodeErrorDTO.getErrorMessage());
      }
      try (OutputStream outputStream = new FileOutputStream(filePath)) {
        xssfWorkbook.write(outputStream);
      }
    }
  }

  private static void populateHeaderFlags(String qrGenerationType) {
    if (AllowedQRGenerationType.STORE.getValue().equals(qrGenerationType)) {
      isPickupPointNamePresent = true;
    } else if (AllowedQRGenerationType.PRODUCT.getValue().equals(qrGenerationType)) {
      isProductSkuPresent = true;
    } else if (AllowedQRGenerationType.ITEM.getValue().equals(qrGenerationType)) {
      isProductSkuPresent = true;
      isItemSkuPresent = true;
    } else if (AllowedQRGenerationType.ADD_TO_BAG.getValue().equals(qrGenerationType)) {
      isItemSkuPresent = true;
      isPickupPointNamePresent = true;
    } else {
      isProductSkuPresent = true;
      isItemSkuPresent = true;
      isPickupPointNamePresent = true;
    }
  }
}
