package com.gdn.mta.bulk.helper;

import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessage;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant;
import static com.gdn.partners.bulk.util.Constant.BRAND_VALUE_HEADER;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.dto.GenericTemplateDataReadDTO;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.models.download.responsedata.CampaignPriceInfo;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.models.download.responsedata.SkuRebateResponse;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.ExcelHeaderNames;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.poi.hssf.usermodel.HSSFFont;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackagePart;
import org.apache.poi.openxml4j.opc.PackagePartName;
import org.apache.poi.openxml4j.opc.PackagingURIHelper;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.model.SharedStringsTable;
import org.apache.poi.xssf.usermodel.XSSFRichTextString;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.xmlbeans.XmlException;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTCell;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTRow;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTSheet;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTSheetData;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTWorksheet;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.STCellType;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.WorkbookDocument;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.WorksheetDocument;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableSet;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ExcelTemplateUtil {
  private static final String PICKUP_POINTS = "pickupPoints";
  private static final String BRAND = "Brand";
  private static final String CATEGORY_C1 = "C1";
  private static final String CATEGORY_ATTRIBUTE_SHEET = "inputs";
  private static final int CATEGORY_ATTRIBUTE_SHEET_INDEX = 6;
  private static final int ROW_INDEX = 4;
  private static final int COLUMN_C1_C2 = 0;
  private static final int COLUMN_CN = 1;
  private static final int CATEGORY_C1TOC2_COLUMN1 = 3;
  private static final int CATEGORY_C1TOC2_COLUMN2 = 4;
  private static final int C1_COLUMN = 6;
  private static final int ATTRIBUTE_VALUES_COLUMN1 = 8;
  private static final int ATTRIBUTE_VALUES_COLUMN2 = 9;
  private static final int ATTRIBUTE_VALUES_COLUMN3 = 10;
  private static final int ATTRIBUTE_VALUES_COLUMN4 = 11;
  private static final int WARNA_COLUMN = 16;
  private static final int VARIASI_COLUMN = 18;
  private static final int DESCRIPTIVE_ATTR_CN_COLUMN = 26;
  private static final int SHIPPING_TYPE_LIST_COLUMN = 29;
  private static final String BRAND_SHEET = "/xl/worksheets/sheet7.xml";
  private static final String INPUT_SHEET = "/xl/worksheets/sheet3.xml";
  private static final int BRAND_SHEET_ROW = 4;
  private static final int BRAND_SHEET_ROW_HEADER = 1;
  private static final int BRAND_SHEET_COLUMN1 = 0;
  private static final int BRAND_SHEET_COLUMN2 = 1;
  private static final String PICKUP_POINTS_SHEET = "/xl/worksheets/sheet8.xml";
  private static final String SHIPPING_OPTIONS_SHEET = "/xl/worksheets/sheet9.xml";
  private static final int PICKUP_POINTS_COLUMN1 = 0;
  private static final int PICKUP_POINTS_COLUMN2 = 1;
  private static final int PICKUP_POINTS_COLUMN3 = 2;
  private static final String SHARED_STRING = "/xl/sharedStrings.xml";
  private static final int DEFAULT_BRAND_SHEET_ROW = 2;
  private static final int BRAND_SHEET_COLUMN3 = 2;
  private static final String ERROR_HEADER = "Error";
  private static final int EXCEL_VERSION_COLUMN_NUMBER = 1;
  private static final int EXCEL_VERSION_ROW_NUMBER = 3;
  private static final String EXCEL_VERSION_SPLIT_DELIMITER = ", Version : ";
  private static final int PARENT_COLUMN_NUMBER = 14;
  public static final Set<String> BULK_PROCESS_TYPES_FOR_1P =
      ImmutableSet.of(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name(),
          BulkInternalProcessType.BULK_PRICE_REBATE.name(),
          BulkInternalProcessType.BULK_PRICE_UPDATE.name(),
          BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
  public static final String TARGET_BRAND_SHEET_NAME = "All Brands List";
  public static final String WORKBOOK = "/xl/workbook.xml";

  public static boolean checkForDataListAndFlagStatus(List<?> dataList,
    UnifiedBulkDownloadEvent lastDownloadStatus,
      String sheet, boolean isGlobalFlag) {
    if (CollectionUtils.isEmpty(dataList)) {
      return false;
    }
    if (isGlobalFlag || Objects.isNull(lastDownloadStatus) || (StringUtils.equalsIgnoreCase(sheet, PICKUP_POINTS) && lastDownloadStatus.isPickupPointUpdated())
        || (StringUtils.equalsIgnoreCase(sheet, BRAND) && lastDownloadStatus.isBrandUpdated())) {
      return true;
    }
    return false;
  }

  public static byte[] setProductUnifiedTemplatePickupPoints(Map<String, List<?>> datas, byte[] destinationFile,
      boolean isWhitelistedSeller, boolean pickupPointNameConcat, String ppNameDelimiter) throws Exception {
    Runtime runtime = Runtime.getRuntime();
    Date startDate = Calendar.getInstance().getTime();
    long startMemory = runtime.totalMemory() - runtime.freeMemory();
    try (InputStream inputStream = new ByteArrayInputStream(destinationFile);
      OPCPackage opcpackage = OPCPackage.open(inputStream)) {
      PackagePart sheetPart =
        opcpackage.getPart(PackagingURIHelper.createPartName(PICKUP_POINTS_SHEET));

      try (InputStream brandSheetInputStream = sheetPart.getInputStream()) {
        WorksheetDocument worksheetDocument =
          WorksheetDocument.Factory.parse(brandSheetInputStream);
        CTWorksheet worksheet = worksheetDocument.getWorksheet();
        CTSheetData sheetData = worksheet.getSheetData();
        sheetData.setNil();
        setPickUpPointHeaders(sheetData, pickupPointNameConcat, ppNameDelimiter);

        for (PickupPointResponse pickupPoint : (List<PickupPointResponse>) datas.get(PICKUP_POINTS)) {
          CTRow row = sheetData.addNewRow();
          CTCell cell0 = row.insertNewC(PICKUP_POINTS_COLUMN1);
          cell0.setT(STCellType.STR);
          cell0.setV(pickupPoint.getCode());
          CTCell cell1 = null;
          if (pickupPointNameConcat) {
            StringBuilder ppCodeAndName = new StringBuilder();
            cell0.setV(ppCodeAndName.append(pickupPoint.getCode()).append(Constant.SPACE).append(ppNameDelimiter)
                .append(Constant.SPACE).append(pickupPoint.getName()).toString());
          } else {
            cell1 = row.insertNewC(PICKUP_POINTS_COLUMN2);
            cell1.setT(STCellType.STR);
            cell1.setV(pickupPoint.getName());
          }
          if (isWhitelistedSeller && pickupPoint.isFbbActivated()) {
            CTCell cell2 = row.insertNewC(pickupPointNameConcat ? PICKUP_POINTS_COLUMN2 : PICKUP_POINTS_COLUMN3);
            cell2.setT(STCellType.STR);
            cell2.setV(Constant.FBB);
          }
        }

        Date endDate = Calendar.getInstance().getTime();
        long endMemory = runtime.totalMemory() - runtime.freeMemory();

        log.debug("Pickup point sheet Time in ms : " + (endDate.getTime() - startDate.getTime()));
        log.debug(
          "Pickup point sheet Memory in MB : " + ((endMemory - startMemory) / (1024 * 1024)));

        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
          OutputStream outputStream = sheetPart.getOutputStream()) {
          bos.write(worksheetDocument.xmlText().getBytes(StandardCharsets.UTF_8));
          outputStream.write(bos.toByteArray());
        }
      }
      try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
        opcpackage.save(bos);
        return bos.toByteArray();
      }
    }
  }

  public static void setPickUpPointHeaders(CTSheetData sheetData, boolean pickupPointNameConcat,
      String ppNameDelimiter) {
    CTRow row = sheetData.addNewRow();
    CTCell HeaderCell = row.insertNewC(PICKUP_POINTS_COLUMN1);
    HeaderCell.setT(STCellType.STR);
    HeaderCell.setV(Constant.PICKUP_POINT_SHEET_HEADER);
    sheetData.addNewRow();
    row=sheetData.addNewRow();
    CTCell cell0 = row.insertNewC(PICKUP_POINTS_COLUMN1);
    cell0.setT(STCellType.STR);
    if (pickupPointNameConcat) {
      StringBuilder ppCodeAndName = new StringBuilder();
      cell0.setV(ppCodeAndName.append(Constant.PICKUP_POINT_CODE_HEADER).append(Constant.SPACE).append(ppNameDelimiter)
          .append(Constant.SPACE).append(Constant.PICKUP_POINT_NAME_HEADER).toString());
    } else {
      cell0.setV(Constant.PICKUP_POINT_CODE_HEADER);
    }
  }


  public static byte[] regenerateBrandValuesSheet(Map<String, List<String>> categoryBrandMapping, byte[] fileByteData)
      throws IOException, XmlException, InvalidFormatException {
    try (InputStream inputStream = new ByteArrayInputStream(fileByteData);
      OPCPackage opcpackage = OPCPackage.open(inputStream)) {
      PackagePart sheetPart = opcpackage.getPart(PackagingURIHelper.createPartName(BRAND_SHEET));
      WorksheetDocument worksheetDocument = WorksheetDocument.Factory.parse(sheetPart.getInputStream());
        CTWorksheet worksheet = worksheetDocument.getWorksheet();
        CTSheetData sheetData = worksheet.getSheetData();
        AtomicInteger rowIndex = new AtomicInteger(BRAND_SHEET_ROW_HEADER);
        sheetData.setNil();
        setBrandHeaders(sheetData, rowIndex);
        for (Map.Entry<String, List<String>> categoryMapping : categoryBrandMapping.entrySet()) {
          List<String> brandValues = categoryBrandMapping.get(categoryMapping.getKey());
          for (String brand : brandValues) {
            CTRow ctRow = sheetData.insertNewRow(rowIndex.get() + 1);
            CTCell categoryNameColumn = ctRow.insertNewC(BRAND_SHEET_COLUMN1);
            categoryNameColumn.setT(STCellType.STR);
            categoryNameColumn.setV(categoryMapping.getKey());
            log.info("regenerateBrandValuesSheet for Gcs for categoryNameColumn :{} ",
              categoryNameColumn);
            CTCell brandValueColumn = ctRow.insertNewC(BRAND_SHEET_COLUMN2);
            brandValueColumn.setT(STCellType.STR);
            brandValueColumn.setV(brand);
            rowIndex.getAndIncrement();
          }
        }
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
          OutputStream outputStream = sheetPart.getOutputStream()) {
          bos.write(worksheetDocument.xmlText().getBytes(StandardCharsets.UTF_8));
          outputStream.write(bos.toByteArray());
        }
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
          opcpackage.save(bos);
          return bos.toByteArray();
        }
      }
    }

  private static void setBrandHeaders(CTSheetData sheetData, AtomicInteger rowIndex) {
    sheetData.addNewRow();
    CTRow row = sheetData.insertNewRow(rowIndex.get() + 1);
    CTCell brandHeader = row.insertNewC(BRAND_SHEET_COLUMN1);
    brandHeader.setT(STCellType.STR);
    brandHeader.setV(BRAND_VALUE_HEADER);
    rowIndex.getAndIncrement();
    CTRow secondRow = sheetData.insertNewRow(rowIndex.get() + 1);
    CTCell columnNames = secondRow.insertNewC(BRAND_SHEET_COLUMN1);
    columnNames.setT(STCellType.STR);
    columnNames.setV(Constant.C1_CATEGORY_COLUMN);
    CTCell columnNames1 = secondRow.insertNewC(1);
    columnNames1.setT(STCellType.STR);
    columnNames1.setV(Constant.BRAND_COLUMN);
    rowIndex.getAndIncrement();
  }

  public static byte[] setProductUnifiedTemplateBrandValues(Map<String, List<?>> datas,
    byte[] fileByteData) throws Exception {
    List<PredefinedAllowedAttributeValueResponse> inReviewBrand =
      (List<PredefinedAllowedAttributeValueResponse>) datas.get(BRAND);
    Runtime runtime = Runtime.getRuntime();
    Date startDate = Calendar.getInstance().getTime();
    long startMemory = runtime.totalMemory() - runtime.freeMemory();
    List<String> dataList = new ArrayList<>();
    for (PredefinedAllowedAttributeValueResponse predefinedAttribute : inReviewBrand) {
      dataList.add(predefinedAttribute.getValue() + Constant.BRAND_IN_REVIEW);
    }
    try (InputStream inputStream = new ByteArrayInputStream(fileByteData);
      OPCPackage opcpackage = OPCPackage.open(inputStream)) {
      PackagePart sheetPart = opcpackage.getPart(PackagingURIHelper.createPartName(BRAND_SHEET));
      try (InputStream brandSheetInputStream = sheetPart.getInputStream()) {
        WorksheetDocument worksheetDocument =
          WorksheetDocument.Factory.parse(brandSheetInputStream);
        CTWorksheet worksheet = worksheetDocument.getWorksheet();
        CTSheetData sheetData = worksheet.getSheetData();


    CTRow[] ctRows = sheetData.getRowArray();
    AtomicInteger rowIndex = new AtomicInteger(BRAND_SHEET_ROW);
    AtomicReference<String> previous = new AtomicReference<>(StringUtils.EMPTY);
    Map<String, String> inReviewBrandInSheet = new HashMap<>();
    log.debug("Row length {}", ctRows.length);
    IntStream.range(BRAND_SHEET_ROW, ctRows.length).forEach(index -> {
      String cell0Value = ctRows[index].getCArray()[BRAND_SHEET_COLUMN1].getV();
      String cell1Value = ctRows[index].getCArray()[BRAND_SHEET_COLUMN2].getV();

      log.debug(cell0Value + " : " + cell1Value);
      log.debug(
          ctRows[index].getCArray()[BRAND_SHEET_COLUMN1].getT() + " : " + ctRows[index].getCArray()[BRAND_SHEET_COLUMN1]
              .getT());
      if (StringUtils.equalsIgnoreCase(previous.get(), StringUtils.EMPTY)) {
        previous.set(cell0Value);
      }
      if (StringUtils.containsIgnoreCase(cell1Value, Constant.BRAND_IN_REVIEW) && !inReviewBrandInSheet
          .containsKey(cell1Value)) {
        inReviewBrandInSheet.put(cell1Value, Constant.BRAND_IN_REVIEW);
      }
      if (!StringUtils.equalsIgnoreCase(previous.get(), cell0Value)) {
        for (String reviewBrand : dataList) {
          if ( !inReviewBrandInSheet.containsKey(reviewBrand)) {
            CTRow row = sheetData.insertNewRow(rowIndex.get());
            CTCell cell0 = row.insertNewC(BRAND_SHEET_COLUMN1);
            cell0.setT(STCellType.STR);
            cell0.setV(previous.get());

            CTCell cell1 = row.insertNewC(BRAND_SHEET_COLUMN2);
            cell1.setT(STCellType.STR);
            cell1.setV(reviewBrand);
            rowIndex.getAndIncrement();
          }
        }
        previous.set(cell0Value);
      }
      rowIndex.getAndIncrement();
    });

    for (String reviewBrand : dataList) {
      if ( !inReviewBrandInSheet.containsKey(reviewBrand)) {
        CTRow row = sheetData.insertNewRow(rowIndex.get() + 1);
        CTCell cell0 = row.insertNewC(BRAND_SHEET_COLUMN1);
        cell0.setT(STCellType.STR);
        cell0.setV(previous.get());

        CTCell cell1 = row.insertNewC(BRAND_SHEET_COLUMN2);
        cell1.setT(STCellType.STR);
        cell1.setV(reviewBrand);
        rowIndex.getAndIncrement();
      }
    }

        Date endDate = Calendar.getInstance().getTime();
        long endMemory = runtime.totalMemory() - runtime.freeMemory();
        log.debug("Brand sheet Time in ms : " + (endDate.getTime() - startDate.getTime()));
        log.debug("Brand sheet Memory in MB : " + ((endMemory - startMemory) / (1024 * 1024)));

        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
          OutputStream outputStream = sheetPart.getOutputStream()) {
          bos.write(worksheetDocument.xmlText().getBytes(StandardCharsets.UTF_8));
          outputStream.write(bos.toByteArray());
        }
      }
      try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
        opcpackage.save(bos);
        return bos.toByteArray();
      }
    }
  }

  public static XSSFWorkbook getProductUnifiedBaseTemplate(String blibliMassTemplateLocation,
      String unifiedBaseTemplateFile, String unifiedUploadTemplateFile, boolean isEnglishTemplate) throws Exception {
    File uploadTemplateFile = new File(blibliMassTemplateLocation + Constant.SLASH + unifiedUploadTemplateFile);
    XSSFWorkbook workBook;
    if (uploadTemplateFile.exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(uploadTemplateFile))) {
        workBook = new XSSFWorkbook(is);
        regenerateValueHeaderSheet(workBook, isEnglishTemplate);
        return workBook;
      }
    } else {
      try (InputStream is = new BufferedInputStream(Files.newInputStream(
        new File(blibliMassTemplateLocation + Constant.SLASH + unifiedBaseTemplateFile).toPath()))) {
        workBook = new XSSFWorkbook(is);
        return workBook;
      }
    }
  }

  public static void regenerateValueHeaderSheet(XSSFWorkbook workBook, boolean isEnglishTemplate) {
    workBook.removeSheetAt(workBook.getSheetIndex(CATEGORY_ATTRIBUTE_SHEET));
    XSSFSheet sheet = createInputSheet(workBook);
    CellStyle borderCellStyle = getBorderCellStyle(workBook);
    CellStyle headerFontStyle = getHeaderFont(workBook);
    createInputHeaderRow(sheet, headerFontStyle);
    createInputSubHeaderRow(sheet, borderCellStyle);
    addFamilyColourValues(sheet);
    addProductTypes(sheet, isEnglishTemplate);
    addWarnaFamilyColourMappingValues(sheet);
    workBook.setSheetHidden(GenericBulkParameters.CATEGORY_ATTRIBUTE_SHEET_INDEX, XSSFWorkbook.SHEET_STATE_VERY_HIDDEN);
  }

  private static void addWarnaFamilyColourMappingValues(XSSFSheet sheet) {
    for (int warnaFamilyColourMapping = 0; warnaFamilyColourMapping < GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING
        .size(); warnaFamilyColourMapping++) {
      if (Objects.isNull(sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping))) {
        sheet.createRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping);
      }
      sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
          .createCell(GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING_HEADER_COLUMN).setCellValue(
          GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[0]);
      sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
          .createCell(GenericBulkParameters.FAMILY_COLOUR_MAPPING_HEADER_COLUMN).setCellValue(
          GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[1]);
    }
  }

  private static void addProductTypes(XSSFSheet sheet, boolean isEnglishTemplate) {
    List<String> shippingType = BulkParameters.BULK_OPTION_UPLOAD_SUPPORT;
    if (isEnglishTemplate) {
      shippingType = BulkParameters.BULK_OPTION_UPLOAD_SUPPORT_EN;
    }
    for (int productType = 0; productType < shippingType.size(); productType++) {
      if (Objects.isNull(sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + productType))) {
        sheet.createRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + productType);
      }
      sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + productType)
          .createCell(GenericBulkParameters.HANDLING_TYPE_HEADER_COLUMN)
          .setCellValue(shippingType.get(productType));
    }
  }

  private static void addFamilyColourValues(XSSFSheet sheet) {
    for (int familyColour = 0; familyColour < GenericBulkParameters.FAMILY_COLOUR_VALUE.size(); familyColour++) {
      sheet.createRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + familyColour)
          .createCell(GenericBulkParameters.FAMILY_COLOUR_DATA_COLUMN)
          .setCellValue(GenericBulkParameters.FAMILY_COLOUR_VALUE.get(familyColour));
    }
  }

  private static void createInputSubHeaderRow(XSSFSheet sheet, CellStyle borderCellStyle) {
    for (int subHeader = 0; subHeader < GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.size(); subHeader++) {
      sheet.getRow(GenericBulkParameters.SUB_HEADER_ROW)
          .createCell(GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.get(subHeader))
          .setCellValue(GenericBulkParameters.SUB_HEADER_COLUMN_VALUE.get(subHeader));
      sheet.getRow(GenericBulkParameters.SUB_HEADER_ROW)
          .getCell(GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.get(subHeader)).setCellStyle(borderCellStyle);
    }
  }

  private static void createInputHeaderRow(XSSFSheet sheet, CellStyle headerFontStyle) {
    for (int header = 0; header < GenericBulkParameters.HEADER_COLUMN_NUMBER.size(); header++) {
      sheet.getRow(GenericBulkParameters.HEADER_ROW).createCell(GenericBulkParameters.HEADER_COLUMN_NUMBER.get(header))
          .setCellValue(GenericBulkParameters.HEADER_COLUMN_VALUE.get(header));
      sheet.getRow(GenericBulkParameters.HEADER_ROW).getCell(GenericBulkParameters.HEADER_COLUMN_NUMBER.get(header))
          .setCellStyle(headerFontStyle);
    }
  }

  private static XSSFSheet createInputSheet(XSSFWorkbook workBook) {
    XSSFSheet sheet = workBook.createSheet(CATEGORY_ATTRIBUTE_SHEET);
    workBook.setSheetOrder(CATEGORY_ATTRIBUTE_SHEET, CATEGORY_ATTRIBUTE_SHEET_INDEX);
    sheet.createRow(GenericBulkParameters.HEADER_ROW);
    sheet.createRow(GenericBulkParameters.SUB_HEADER_ROW);
    sheet.setDisplayGridlines(false);
    return sheet;
  }

  private static CellStyle getHeaderFont(XSSFWorkbook workBook) {
    CellStyle style = workBook.createCellStyle();
    Font headerFont = workBook.createFont();
    headerFont.setUnderline(HSSFFont.U_SINGLE);
    headerFont.setBold(true);
    style.setFont(headerFont);
    return style;
  }

  private static CellStyle getBorderCellStyle(XSSFWorkbook workBook) {
    CellStyle style = workBook.createCellStyle();
    style.setBorderBottom(BorderStyle.THIN);
    style.setBottomBorderColor(IndexedColors.GREY_40_PERCENT.getIndex());
    style.setBorderLeft(BorderStyle.THIN);
    style.setLeftBorderColor(IndexedColors.GREY_40_PERCENT.getIndex());
    style.setBorderRight(BorderStyle.THIN);
    style.setRightBorderColor(IndexedColors.GREY_40_PERCENT.getIndex());
    style.setBorderTop(BorderStyle.THIN);
    style.setTopBorderColor(IndexedColors.GREY_40_PERCENT.getIndex());
    return style;
  }

  public static void regenerateCategorySheet(Map<String, List<?>> datas,
      Map<String, List<CategoryTreeResponse>> categoryMapping, Map<String, List<String>> mapC2ToCn,
      XSSFWorkbook workbook, boolean isEnglishTemplate) {
    XSSFSheet categorySheet = workbook.getSheet(CATEGORY_ATTRIBUTE_SHEET);
    setProductUnifiedBaseTemplateCategoryC1Values(datas, categorySheet);
    setProductUnifiedBaseTemplateCategoryC1MappingC2Values(categoryMapping, categorySheet, isEnglishTemplate);
    setProductUnifiedBaseTemplateCategoryC2MappingCn(mapC2ToCn, categorySheet, ROW_INDEX, COLUMN_C1_C2, COLUMN_CN);
  }

  private static void setProductUnifiedBaseTemplateCategoryC2MappingCn(Map<String, List<String>> categoryMap,
      XSSFSheet categorySheet, int rowIndex, int column1, int column2) {
    List<String> categoryList =
        categoryMap.keySet().stream().sorted(String.CASE_INSENSITIVE_ORDER).collect(Collectors.toList());
    int i = rowIndex;
    for (String category : categoryList) {
      if (CollectionUtils.isNotEmpty(categoryMap.get(category))) {
        List<String> sortedCategoryCn = categoryMap.get(category).stream().sorted(String.CASE_INSENSITIVE_ORDER).collect(
            Collectors.toList());
        for (String colValue : sortedCategoryCn) {
          XSSFRow row = categorySheet.getRow(i);
          if (Objects.isNull(row)) {
            categorySheet.createRow(i);
            row = categorySheet.getRow(i);
          }
          row.createCell(column1);
          row.createCell(column2);
          row.getCell(column1).setCellValue(category);
          row.getCell(column2).setCellValue(colValue);
          i++;
        }
      } else {
        XSSFRow row = categorySheet.getRow(i);
        if (Objects.isNull(row)) {
          categorySheet.createRow(i);
          row = categorySheet.getRow(i);
        }
        row.createCell(column1);
        row.createCell(column2);
        row.getCell(column1).setCellValue(category);
        row.getCell(column2).setCellValue(GenericBulkParameters.NOT_APPLICABLE);
        i++;
      }
    }
  }

  public static void setProductUnifiedBaseTemplateTwoColumnValues(Map<String, List<String>> categoryMap,
      XSSFSheet categorySheet, int rowIndex, int column1, int column2) {
    List<String> categoryList = categoryMap.keySet().stream().sorted(String.CASE_INSENSITIVE_ORDER)
        .collect(Collectors.toList());
    int i = rowIndex;
    for (String category : categoryList) {
      for (String colValue : categoryMap.get(category)) {
        XSSFRow row = categorySheet.getRow(i);
        if (Objects.isNull(row)) {
          categorySheet.createRow(i);
          row = categorySheet.getRow(i);
        }
        row.createCell(column1);
        row.createCell(column2);
        row.getCell(column1).setCellValue(category);
        row.getCell(column2).setCellValue(colValue);
        i++;
      }
    }
  }

  private static void setProductUnifiedBaseTemplateCategoryC1MappingC2Values(
      Map<String, List<CategoryTreeResponse>> categoryC1NameC2NameMap, XSSFSheet categorySheet,
      boolean isEnglishTemplate) {
    int column_index_c1 = CATEGORY_C1TOC2_COLUMN1;
    int column_index_c2 = CATEGORY_C1TOC2_COLUMN2;
    List<String> categoryC1List =
        categoryC1NameC2NameMap.keySet().stream().sorted(String.CASE_INSENSITIVE_ORDER).collect(Collectors.toList());
    int i = ROW_INDEX;
    for (String category : categoryC1List) {
      List<CategoryTreeResponse> sortedCategoryTreeResponse = categoryC1NameC2NameMap.get(category).stream()
          .sorted(Comparator.comparing(CategoryTreeResponse::getCategoryName)).collect(Collectors.toList());
      for (CategoryTreeResponse colValue : sortedCategoryTreeResponse) {
        XSSFRow row = categorySheet.getRow(i);
        if (Objects.isNull(row)) {
          categorySheet.createRow(i);
          row = categorySheet.getRow(i);
        }
        row.createCell(column_index_c1);
        row.createCell(column_index_c2);
        row.getCell(column_index_c1).setCellValue(category);
        if (isEnglishTemplate && StringUtils.isNotBlank(colValue.getCategoryEnglishName())) {
          row.getCell(column_index_c2).setCellValue(colValue.getCategoryEnglishName());
        } else {
          row.getCell(column_index_c2).setCellValue(colValue.getCategoryName());
        }

        i++;
      }
    }
  }

  public static void setProductUnifiedBaseTemplateCategoryC1Values(Map<String, List<?>> datas, XSSFSheet sheet) {

    List<String> categoryC1List = (List<String>) datas.get(CATEGORY_C1);
    if (CollectionUtils.isNotEmpty(categoryC1List)) {
      Collections.sort(categoryC1List, String.CASE_INSENSITIVE_ORDER);
      setProductUnifiedBaseTemplateListForSingleColumn(categoryC1List, sheet, ROW_INDEX, C1_COLUMN);
    }

    List<String> warnaCategoryList = (List<String>) datas.get(BulkParameters.WARNA_CN_CATEGORY_MAPPING);
    if (CollectionUtils.isNotEmpty(datas.get(BulkParameters.WARNA_CN_CATEGORY_MAPPING))) {
      Collections.sort(warnaCategoryList, String.CASE_INSENSITIVE_ORDER);
      setProductUnifiedBaseTemplateListForSingleColumn(warnaCategoryList, sheet, ROW_INDEX, WARNA_COLUMN);
    }

    List<String> variasiList = (List<String>) datas.get(BulkParameters.VARIASI_CN_CATEGORY_MAPPING);
    if (CollectionUtils.isNotEmpty(variasiList)) {
      Collections.sort(variasiList, String.CASE_INSENSITIVE_ORDER);
      setProductUnifiedBaseTemplateListForSingleColumn(variasiList, sheet, ROW_INDEX, VARIASI_COLUMN);
    }

    List<String> descriptiveCnMappingList =
        (List<String>) datas.get(BulkParameters.DESCRIPTIVE_ATTR_CN_CATEGORY_MAPPING);
    if (CollectionUtils.isNotEmpty(descriptiveCnMappingList)) {
      Collections.sort(descriptiveCnMappingList, String.CASE_INSENSITIVE_ORDER);
      setProductUnifiedBaseTemplateListForSingleColumn(descriptiveCnMappingList, sheet, ROW_INDEX,
          DESCRIPTIVE_ATTR_CN_COLUMN);
    }

    List<String> shippingTypeList =
        (List<String>) datas.get(BulkParameters.SHIPPING_TYPE_LIST);
    if (CollectionUtils.isNotEmpty(shippingTypeList)) {
      setProductUnifiedBaseTemplateListForSingleColumn(shippingTypeList, sheet, ROW_INDEX,
          SHIPPING_TYPE_LIST_COLUMN);
    }
  }

  private static void setProductUnifiedBaseTemplateListForSingleColumn(List<String> categoryList, XSSFSheet sheet,
      int rowIndex, int columnIndex) {
    int i = rowIndex;
    for (String colValue : categoryList) {
      XSSFRow row = sheet.getRow(i);
      if (Objects.isNull(row)) {
        sheet.createRow(i);
        row = sheet.getRow(i);
      }
      row.createCell(columnIndex);
      row.getCell(columnIndex).setCellValue(colValue);
      i++;
    }
  }

  public static void writeGenericTemplate(String blibliMassTemplateLocation, String unifiedUploadTemplateFile,
      XSSFWorkbook workbook) throws Exception {
    try (OutputStream os = new FileOutputStream(
        new File(blibliMassTemplateLocation + Constant.SLASH + unifiedUploadTemplateFile))) {
      workbook.write(os);
    }
  }

  public static int regenerateCategoryAttributeList(XSSFWorkbook workbook, String nameMapping, String name,
      List<String> predefinedValues, int row_index) {
    int indexColumn_I = ATTRIBUTE_VALUES_COLUMN1;
    int indexColumn_J = ATTRIBUTE_VALUES_COLUMN2;
    int indexColumn_K = ATTRIBUTE_VALUES_COLUMN3;
    int indexColumn_L = ATTRIBUTE_VALUES_COLUMN4;
    XSSFSheet sheet = workbook.getSheet(CATEGORY_ATTRIBUTE_SHEET);
    for (String value : predefinedValues) {
      XSSFRow row = sheet.getRow(row_index);
      if (Objects.isNull(row)) {
        sheet.createRow(row_index);
        row = sheet.getRow(row_index);
      }
      row.createCell(indexColumn_I);
      row.createCell(indexColumn_J);
      row.createCell(indexColumn_K);
      row.createCell(indexColumn_L);
      row.getCell(indexColumn_I).setCellValue(nameMapping);
      row.getCell(indexColumn_J).setCellValue(name);
      row.getCell(indexColumn_K).setCellValue(value);
      row.getCell(indexColumn_L).setCellValue(nameMapping + Constant.UNDERSCORE + name);
      row_index++;
    }
    return row_index;
  }

  public static void readGenericTemplateData(GenericTemplateDataReadDTO genericTemplateDataReadDTO) throws Exception {
    List<List<Object>> userInputRows = genericTemplateDataReadDTO.getUserInputRows();
    MerchantStatusType merchantStatusType = genericTemplateDataReadDTO.getMerchantStatusType();
    byte[] fileByteData = genericTemplateDataReadDTO.getFileByteData();
    String merchantType = genericTemplateDataReadDTO.getMerchantType();
    boolean productBundlingEnabled = genericTemplateDataReadDTO.isProductBundlingEnabled();
    String productBundlingEligibleMerchantTypes = genericTemplateDataReadDTO.getProductBundlingEligibleMerchantTypes();
    boolean genericFileHeaderValidationEn = genericTemplateDataReadDTO.isGenericFileHeaderValidationEn();
    Boolean internationalMerchant = genericTemplateDataReadDTO.getInternationalMerchant();
    List<Object> excelBahasaHeaderList = genericTemplateDataReadDTO.getExcelBahasaHeaderList();
    List<Object> excelEnglishHeaderList = genericTemplateDataReadDTO.getExcelEnglishHeaderList();
    try (OPCPackage opcpackage = OPCPackage.open(new ByteArrayInputStream(fileByteData))) {
      PackagePart sharedStringsTablePart =
        opcpackage.getPartsByName(Pattern.compile(SHARED_STRING)).get(0);
      PackagePart sheetPart = opcpackage.getPartsByName(Pattern.compile(INPUT_SHEET)).get(0);
      SharedStringsTable sharedStringsTable = new SharedStringsTable();
      sharedStringsTable.readFrom(sharedStringsTablePart.getInputStream());
      WorksheetDocument worksheetDocument = WorksheetDocument.Factory.parse(sheetPart.getInputStream());
      CTWorksheet worksheet = worksheetDocument.getWorksheet();
      CTSheetData sheetData = worksheet.getSheetData();
      int columnEndNumber = GenericBulkParameters.COLUMN_END_NUMBER + merchantStatusType.getIncrementBy();
      if(isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes, productBundlingEnabled)) {
        columnEndNumber = columnEndNumber + GenericBulkParameters.BUNDLE_COLUMN_NUMBERS;
      }
      if(genericTemplateDataReadDTO.isInstoreEligible()) {
        columnEndNumber = columnEndNumber + GenericBulkParameters.INSTORE_COLUMN_NUMBERS;
      }
      CTRow[] ctRows = sheetData.getRowArray();
      //excel version validation
      if (genericTemplateDataReadDTO.isBulkExcelVersioningEn()) {
        validateExcelVersion(genericTemplateDataReadDTO, internationalMerchant, sharedStringsTable, ctRows);
      }
      if (genericFileHeaderValidationEn) {
        //Extract English headers
        int headerRow = GenericBulkParameters.genericExcelHeaderRowNumberMap.get(true);
        extractExcelHeaders(internationalMerchant, columnEndNumber, headerRow, ctRows, excelEnglishHeaderList,
            sharedStringsTable);
        //Extract Bahasa headers
        headerRow = GenericBulkParameters.genericExcelHeaderRowNumberMap.get(false);
        extractExcelHeaders(internationalMerchant, columnEndNumber, headerRow, ctRows, excelBahasaHeaderList,
            sharedStringsTable);
      }
      int rowNumber = 1;
      for (int index = GenericBulkParameters.HEADER_END_ROW; index < ctRows.length; index++) {
        List<Object> rowInput = new ArrayList<>();
        try {
          if (Objects.nonNull(ctRows[index].getCArray()[0].getV())) {
            for (int cellIndex = 0; cellIndex < columnEndNumber; cellIndex++) {
              extractCellContentFromExcelRow(sharedStringsTable, rowInput, cellIndex, ctRows[index]);
            }
          } else {
            // Stop reading file 1st column is empty
            break;
          }
        } catch (ArrayIndexOutOfBoundsException e) {
          log.error("Error in reading row because of merged cells for row : {}, fileName : {} ", rowNumber,
              genericTemplateDataReadDTO.getExcelFileName());
          genericTemplateDataReadDTO.getFailedExcelRows().add(rowNumber);
          rowInput = new ArrayList<>();
          if (Objects.nonNull(ctRows[index].getCArray()[0].getV())) {
            for (int cellIndex = 0; cellIndex < PARENT_COLUMN_NUMBER; cellIndex++) {
              extractCellContentFromExcelRow(sharedStringsTable, rowInput, cellIndex, ctRows[index]);
            }
          } else {
            // Stop reading file 1st column is empty
            break;
          }
        } finally {
          rowNumber++;
        }
        userInputRows.add(rowInput);
      }
    }
  }

  private static void validateExcelVersion(GenericTemplateDataReadDTO genericTemplateDataReadDTO,
      Boolean internationalMerchant, SharedStringsTable sharedStringsTable, CTRow[] ctRows) {
    List<Object> excelVersionHeader = new ArrayList<>();
    String excelVersionHeaderValue =
        extractExcelHeaders(internationalMerchant, EXCEL_VERSION_COLUMN_NUMBER, EXCEL_VERSION_ROW_NUMBER, ctRows,
            excelVersionHeader, sharedStringsTable).stream().findFirst().orElse(StringUtils.EMPTY).toString();
    String[] excelVersionArray =
        Splitter.on(EXCEL_VERSION_SPLIT_DELIMITER).limit(2).splitToList(excelVersionHeaderValue).toArray(new String[0]);
    String excelValidVersion = genericTemplateDataReadDTO.getBulkGenericExcelVersion();
    if (genericTemplateDataReadDTO.isInstoreEligible()) {
      excelValidVersion = genericTemplateDataReadDTO.getBulkGenericInstoreExcelVersion();
    }
    if (excelVersionArray.length == 1 || !StringUtils.equalsIgnoreCase(excelVersionArray[1], excelValidVersion)) {
      String errorMessage = genericTemplateDataReadDTO.getExcelFileName().concat(Constant.PERIOD).concat(
          BulkCreationCommonUtil.excelVersionOutdatedErrorMessage.get(
              Optional.of(internationalMerchant).orElse(Boolean.FALSE)).concat(StringUtils.SPACE));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, errorMessage);
    }
  }

  private static List<Object> extractExcelHeaders(boolean isInternationalMerchant, int columnEndNumber, int headerRow,
      CTRow[] ctRows, List<Object> headerList, SharedStringsTable sharedStringsTable) {
    for (int cellIndex = 0; cellIndex < columnEndNumber; cellIndex++) {
      if (Objects.nonNull(ctRows[headerRow].getCArray()[0].getV())) {
        extractCellContentFromExcelRow(sharedStringsTable, headerList, cellIndex, ctRows[headerRow]);
      } else {
        // Stop reading file 1st column is empty
        break;
      }
    }
    return headerList;
  }

  private static void extractCellContentFromExcelRow(SharedStringsTable sharedStringsTable, List<Object> rowInput,
      int cellIndex, CTRow ctRows) {
    CTCell cell0 = ctRows.getCArray()[cellIndex];
    if (StringUtils.isEmpty(cell0.getV())) {
      rowInput.add(StringUtils.EMPTY);
    } else {
      // For cell with text input cell type will be S
      if (STCellType.S.equals(cell0.getT())) {
        rowInput.add(
            new XSSFRichTextString(sharedStringsTable.getEntryAt(Integer.parseInt(cell0.getV()))).toString().trim());
        // For cell with number input cell type will be N like UPC, price, length ...
      } else if (STCellType.N.equals(cell0.getT())) {
        rowInput.add(cell0.getV());
      } else {
        rowInput.add(StringUtils.EMPTY);
      }
    }
  }

  public static boolean isEligibleForBundleCreation(String merchantType, String productBundlingEligibleMerchantTypes,
      boolean productBundlingEnabled) {
    return productBundlingEnabled && Arrays.asList(productBundlingEligibleMerchantTypes.split(Constants.COMMA))
        .contains(merchantType);
  }

  public static byte[] regenerateMasterBrandValuesSheet(List<String> allActiveBrand,
    byte[] fileByteData, boolean useNewBrandSheetReading) throws Exception {
    try (InputStream inputStream = new ByteArrayInputStream(fileByteData);
      OPCPackage opcpackage = OPCPackage.open(inputStream)) {
      PackagePart sheetPart;
      if (useNewBrandSheetReading) {
        PackagePartName workbookPartName = PackagingURIHelper.createPartName(WORKBOOK);
        PackagePart workbookPart = opcpackage.getPart(workbookPartName);

        WorkbookDocument workbookDoc =
          WorkbookDocument.Factory.parse(workbookPart.getInputStream());
        String targetRid = null;
        for (CTSheet sheet : workbookDoc.getWorkbook().getSheets().getSheetArray()) {
          if (TARGET_BRAND_SHEET_NAME.equals(sheet.getName())) {
            targetRid = sheet.getId();
            break;
          }
        }
        if (StringUtils.isEmpty(targetRid)) {
          throw new IllegalArgumentException("Brand sheet not found: " + TARGET_BRAND_SHEET_NAME);
        }
        sheetPart = workbookPart.getRelatedPart(workbookPart.getRelationship(targetRid));
      } else {
        sheetPart = opcpackage.getPartsByName(Pattern.compile(BRAND_SHEET)).get(0);
      }
      try (InputStream brandSheetInputStream = sheetPart.getInputStream()) {
        WorksheetDocument worksheetDocument = WorksheetDocument.Factory.parse(brandSheetInputStream);
        CTWorksheet worksheet = worksheetDocument.getWorksheet();
        CTSheetData sheetData = worksheet.getSheetData();
        sheetData.setNil();
        setBrandHeaders(sheetData);
        for (String brand : allActiveBrand) {
          CTRow row = sheetData.addNewRow();
          CTCell categoryNameColumn = row.insertNewC(BRAND_SHEET_COLUMN1);
          categoryNameColumn.setT(STCellType.STR);
          categoryNameColumn.setV(brand);
        }
        CTRow defaultBrandRow = sheetData.getRowArray(DEFAULT_BRAND_SHEET_ROW);
        CTCell defaultBrandCell = defaultBrandRow.insertNewC(BRAND_SHEET_COLUMN2);
        defaultBrandCell.setT(STCellType.STR);
        defaultBrandCell.setV(Constant.DEFAULT_BRAND_DESCRIPTION_EN);
        CTCell defaultBrandCell1 = defaultBrandRow.insertNewC(BRAND_SHEET_COLUMN3);
        defaultBrandCell1.setT(STCellType.STR);
        defaultBrandCell1.setV(Constant.DEFAULT_BRAND_DESCRIPTION);
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
          OutputStream outputStream = sheetPart.getOutputStream()) {
          bos.write(worksheetDocument.xmlText().getBytes(StandardCharsets.UTF_8));
          outputStream.write(bos.toByteArray());
        }
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
          opcpackage.save(bos);
          return bos.toByteArray();

        }
      }
    }
  }

  private static void setBrandHeaders(CTSheetData sheetData) {
    CTRow row = sheetData.addNewRow();
    CTCell HeaderCell = row.insertNewC(BRAND_SHEET_COLUMN1);
    HeaderCell.setT(STCellType.STR);
    HeaderCell.setV(Constant.BRAND_VALUE_HEADER);
    sheetData.addNewRow();
  }

  public static boolean downloadImages(String bulkProcessCode, Map<String, String> imagesToDownload,
      int httpConnectionTimeout, int httpConnectionReadTimeout, Set<String> urlImagesWithInvalidExtension,
      List<String> images, BulkUploadErrorCounter bulkUploadErrorCounter, StringBuilder validationErrorMessage,
      boolean isInternationalMerchant) {
    boolean result = true;
    for (Map.Entry<String, String> image : imagesToDownload.entrySet()) {
      String path =
          ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + File.separator + ProcessorUtils.DATA_RAW_DIR + File.separator
              + image.getValue();
      log.info("Downloading image from : {} for bulkProcessCode : {} ", image.getKey(), bulkProcessCode);
      try {
        URL urlConnection = new URL(image.getKey());
        Path target = Paths.get(String.valueOf(path));
        URLConnection connection = urlConnection.openConnection();
        connection.setConnectTimeout(httpConnectionTimeout);
        connection.setReadTimeout(httpConnectionReadTimeout);
        connection.setRequestProperty(Constant.ACCEPT_HEADER, Constant.IMAGE_HEADER);
        if (!Constant.ALLOWED_IMAGE_MIME_TYPE.contains(connection.getContentType())) {
          urlImagesWithInvalidExtension.add(image.getKey());
          images.remove(image.getValue());
          result = false;
          bulkUploadErrorCounter.incrementImage();
          if (bulkUploadErrorCounter.getImage() <= Constant.ERROR_COUNT) {
            validationErrorMessage.append(errorMessage(image.getKey() + " - ",
                    errorMessageBasedOnMerchant(isInternationalMerchant,
                        BulkProcessValidationErrorMessages.IMAGE_URL_TYPE_INVALID_EN,
                        BulkProcessValidationErrorMessages.IMAGE_URL_TYPE_INVALID), StringUtils.EMPTY))
                .append(Constant.DOT);
          }
          log.error("Not able to download image : {} for bulkProcessCode : {} invalid type ", image.getKey(),
              bulkProcessCode);
        }
        if (result) {
          try (InputStream in = connection.getInputStream()) {
            // Check here to make sure the URL entered in excel is the same source from which we are downloading
            // there are cases in which re-direction will cause issues, for example
            // For wrong URL instead of 404, a default image is given in response, and this will cause issue.
            Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING);
          }
        }
        if (result) {
          log.info("Image download complete for : {} for bulkProcessCode : {} ", image.getKey(), bulkProcessCode);
        }
      } catch (Exception e) {
        log.error("Not able to download image : {} for bulkProcessCode : {} ", image.getKey(), bulkProcessCode, e);
        result = false;
      }
    }
    return result;
  }

  public static List<List<List<Object>>> generateUniqueParentRows(List<List<Object>> listOfRows,
      List<Integer> ignoredIndex, int parentColumnIndex) {
    listOfRows.forEach(excelRaw -> ignoredIndex.stream().mapToInt(Integer::intValue).forEach(excelRaw::remove));
    // Flag row index information to listOfRows i.e adding row number to each entry 0, 1, 2...
    int rowNumber = 0;
    for (List<Object> row : listOfRows) {
      row.add(String.valueOf(rowNumber));
      rowNumber++;
    }
    List<List<List<Object>>> uniqueParentRows = new ArrayList<>();
    Integer parentIndex = parentColumnIndex;
    List<String> alreadyMappedParent = new ArrayList<>();
    for (List<Object> row : listOfRows) {
      String parentValue = String.valueOf(row.get(parentIndex));
      if (StringUtils.isBlank(parentValue)) {
        List<List<Object>> uniqueRow = new ArrayList<>();
        uniqueRow.add(row);
        uniqueParentRows.add(uniqueRow);
      } else {
        if (!alreadyMappedParent.contains(parentValue)) {
          List<List<Object>> uniqueRow = new ArrayList<>();
          for (List<Object> sameParentRow : listOfRows) {
            String parent = String.valueOf(sameParentRow.get(parentIndex));
            if (parent.equals(parentValue)) {
              uniqueRow.add(sameParentRow);
            }
          }
          alreadyMappedParent.add(parentValue);
          uniqueParentRows.add(uniqueRow);
        }
      }
    }
    return uniqueParentRows;
  }

  public static boolean downloadAndValidateProductCreationImages(BulkProcess bulkProcess,
      BulkUploadErrorCounter bulkUploadErrorCounter, Map<String, String> imageAndImageUrlReverseMap,
      StringBuilder validationErrorMessage, List<String> images, String columnRowInformation, int imageMaxSize,
      boolean isInternationalMerchant, boolean decreaseImageResolution) throws IOException {
    boolean result = true;
    for (String image : images) {
      List<File> files = ProcessorUtils.searchFile(
          ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator
              + ProcessorUtils.DATA_RAW_DIR, image);
      if (files.isEmpty()) {
        result = false;
        bulkUploadErrorCounter.incrementImage();
        String imageFileNameInExcel = image;
        if (imageAndImageUrlReverseMap.containsKey(image)) {
          imageFileNameInExcel = imageAndImageUrlReverseMap.get(image);
        }
        BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getImage(), validationErrorMessage,
            errorMessage(imageFileNameInExcel + " - ", errorMessageBasedOnMerchant(isInternationalMerchant,
                BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND_EN,
                BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND), StringUtils.EMPTY));
        log.error("Bulk process: {}, Row {} : {} - {}.", bulkProcess.getBulkProcessCode(),
            (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.FILE_NOT_FOUND.getDescription(),
            BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND);
      } else {
        String[] imageSplit = image.split("\\.");
        String imageType = imageSplit[imageSplit.length - 1];
        if (!Constant.ALLOWED_IMAGE_TYPE.contains(imageType.toLowerCase())) {
          result = false;
          bulkUploadErrorCounter.incrementImage();
          String imageFileNameInExcel = image;
          if (imageAndImageUrlReverseMap.containsKey(image)) {
            imageFileNameInExcel = imageAndImageUrlReverseMap.get(image);
          }
          BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getImage(), validationErrorMessage,
              errorMessage(image + " - ", errorMessageBasedOnMerchant(isInternationalMerchant,
                  BulkProcessValidationErrorMessages.IMAGE_FILE_TYPE_INVALID_EN,
                  BulkProcessValidationErrorMessages.IMAGE_FILE_TYPE_INVALID), StringUtils.EMPTY));
          log.error("Bulk process : {}, Row {} : {} - {}.", bulkProcess.getBulkProcessCode(),
              (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.FILE_TYPE_INVALID.getDescription(),
              BulkProcessValidationErrorMessages.IMAGE_FILE_TYPE_INVALID);
        } else {
          for (File file : files) {
            byte[] bytesArray = com.google.common.io.Files.toByteArray(file);
            int width = 0, height = 0;
            ImageInputStream imageInputStream = ImageIO.createImageInputStream(file);
            Iterator<ImageReader> imageReaders = ImageIO.getImageReaders(imageInputStream);
            if (imageReaders.hasNext()) {
              ImageReader reader = imageReaders.next();
              reader.setInput(imageInputStream, true);
              width = reader.getWidth(0);
              height = reader.getHeight(0);
            }
            try {
              ImageValidator.validateImages(bytesArray, imageMaxSize, width, height, decreaseImageResolution);
            } catch (ApplicationRuntimeException e) {
              result = false;
              if (bulkUploadErrorCounter.getImage() <= Constant.ERROR_COUNT) {
                String imageFileNameInExcel = file.getName();
                if (imageAndImageUrlReverseMap.containsKey(image)) {
                  imageFileNameInExcel = imageAndImageUrlReverseMap.get(image);
                }
                if (e.getErrorMessage()
                    .contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN)) {
                  String commonErrorMessage = isInternationalMerchant ?
                      BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN :
                      BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE;
                  validationErrorMessage.append(errorMessage(imageFileNameInExcel + " - ", commonErrorMessage, ""))
                      .append(Constant.PERIOD);
                } else if (e.getErrorMessage()
                    .contains(BulkProcessValidationErrorMessages.IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN)) {
                  validationErrorMessage.append(errorMessage(imageFileNameInExcel + " - ",
                          errorMessageBasedOnMerchant(isInternationalMerchant,
                              BulkProcessValidationErrorMessages.IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN,
                              BulkProcessValidationErrorMessages.IMAGE_SIZE_VALIDATION_ERR_MESSAGE), StringUtils.EMPTY))
                      .append(Constant.PERIOD);
                }
              }
              bulkUploadErrorCounter.incrementImage();
              log.error("Bulk process : {}, Row {} : {} - {}. Invalid image type for given file : {}",
                  bulkProcess.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
                  BulkErrorCategory.FILE_TYPE_INVALID.getDescription(),
                  BulkProcessValidationErrorMessages.IMAGE_SIZE_VALIDATION_ERR_MESSAGE_EN, e);
            }
          }
        }
      }
    }
    return result;
  }

  public static void uploadImageFiles(Map.Entry<String, String> mappingImageEntry, BulkProcess bulkProcess,
      String mtaSourcePath) throws IOException {
    File destination = new File(mtaSourcePath + File.separator + mappingImageEntry.getValue());
    List<File> files = ProcessorUtils.searchFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR,
        mappingImageEntry.getKey());
    FileUtils.copyFile(files.get(0), destination);
  }

  public static byte[] generateRestrictedKeywordErrorSheet(Sheet sheet,
      List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping, String processType) throws IOException {
    Row headerRow = sheet.getRow(0);
    if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)
        || BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)
        || BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
      headerRow = sheet.getRow(Constant.TWO);
    }
    Cell headerCell = headerRow.createCell(headerRow.getLastCellNum());
    // Setting bold style for Error Column
    CellStyle cellStyle = sheet.getWorkbook().createCellStyle();
    Font font = sheet.getWorkbook().createFont();
    font.setBold(true);
    cellStyle.setFont(font);
    headerCell.setCellValue(ERROR_HEADER);
    if (BULK_PROCESS_TYPES_FOR_1P.contains(processType)) {
      headerCell.setCellStyle(cellStyle);
    }
    setRowValueBasedOnProcessType(sheet, excelRowNumberAndErrorMessageMapping, processType,
      headerRow);
    Set<Integer> errorRowNumber =
        excelRowNumberAndErrorMessageMapping.stream().map(Pair::getLeft)
            .collect(Collectors.toSet());
    List<Integer> rowsToBeDeleted =
      IntStream.rangeClosed(1, sheet.getLastRowNum()).boxed().collect(Collectors.toList());
    rowsToBeDeleted.removeAll(errorRowNumber);
    if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)
      || BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)
      || BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
      rowsToBeDeleted.removeAll(Set.of(1, 2));
    }
    Collections.sort(rowsToBeDeleted, Collections.reverseOrder());
    return BULK_PROCESS_TYPES_FOR_1P.contains(processType) ?
      removeRowFromExcelAndSkipInvalidIndices(rowsToBeDeleted, sheet) :
      removeRowFromExcel(rowsToBeDeleted, sheet);
  }

  private static void setRowValueBasedOnProcessType(Sheet sheet,
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping, String processType,
    Row headerRow) {
    if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)) {
      setRowValuesWithNullChecks(sheet, excelRowNumberAndErrorMessageMapping, headerRow);
    } else {
      setRowValues(sheet, excelRowNumberAndErrorMessageMapping, headerRow);
    }
  }

  private static void setRowValues(Sheet sheet,
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping, Row headerRow) {
    for (Pair<Integer, String> pair : excelRowNumberAndErrorMessageMapping) {
      Row row = sheet.getRow(pair.getLeft());
      Cell rowCell = row.createCell(headerRow.getLastCellNum() - 1);
      rowCell.setCellValue(pair.getRight());
    }
  }

  private static void setRowValuesWithNullChecks(Sheet sheet,
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping, Row headerRow) {
    for (Pair<Integer, String> pair : excelRowNumberAndErrorMessageMapping) {
      if (Objects.isNull(pair) || Objects.isNull(pair.getLeft()) || Objects.isNull(pair.getRight())) {
        continue;
      }
      Row row = sheet.getRow(pair.getLeft());
      if (Objects.isNull(row)) {
        continue;
      }
      Cell rowCell = row.createCell(headerRow.getLastCellNum() - 1);
      if (Objects.nonNull(rowCell)) {
        rowCell.setCellValue(pair.getRight());
      }
    }
  }

  private static byte[] removeRowFromExcel(List<Integer> rowsToBeDeleted, Sheet sheet) throws IOException {
    for (Integer rowsIdx : rowsToBeDeleted) {
      Integer totalsRows = sheet.getLastRowNum();
      Row row = sheet.getRow(rowsIdx);
      if (Objects.nonNull(row)) {
        sheet.removeRow(row);
      }
      if (!Objects.equals(rowsIdx, totalsRows)) {
        sheet.shiftRows(rowsIdx + 1, sheet.getLastRowNum(), -1);
      }
    }
    return POIUtil.getByteContentFromExcel(sheet);
  }



  public static byte[] removeRowFromExcelAndSkipInvalidIndices(List<Integer> rowsToBeDeleted,
    Sheet sheet) throws IOException {

    for (Integer rowIdx : rowsToBeDeleted) {
      if (rowIdx < 0 || rowIdx > sheet.getLastRowNum()) {
        continue;
      }
      Row row = sheet.getRow(rowIdx);
      if (Objects.nonNull(row)) {
        sheet.removeRow(row);
      }
      int lastRowNum = sheet.getLastRowNum();
      if (rowIdx < lastRowNum) {
        try {
          sheet.shiftRows(rowIdx + 1, lastRowNum, -1);
        } catch (IllegalArgumentException e) {
          log.error("Error shifting rows from {} to {}: {}", rowIdx + 1, lastRowNum, e.getMessage());
        }
      } else {
        log.info("Row index {} is the last row, no rows to shift", rowIdx);
      }
    }
    return POIUtil.getByteContentFromExcel(sheet);
  }

  public static byte[] setShippingOptionInProductUnifiedTemplate(byte[] destinationFile,
    ShippingTypeEligibility shippingTypeEligibility, boolean isInternationalMerchant)
    throws Exception {
    log.info("Generating shipping options!!!");
    try (InputStream inputStream = new ByteArrayInputStream(destinationFile);
      OPCPackage opcpackage = OPCPackage.open(inputStream)) {
      PackagePart sheetPart = opcpackage.getPart(PackagingURIHelper.createPartName(
        SHIPPING_OPTIONS_SHEET));

      try (InputStream brandSheetInputStream = sheetPart.getInputStream()) {
        WorksheetDocument worksheetDocument =
          WorksheetDocument.Factory.parse(brandSheetInputStream);
        CTWorksheet worksheet = worksheetDocument.getWorksheet();
        CTSheetData sheetData = worksheet.getSheetData();
        sheetData.setNil();

        List<String> shippingOptionsAvailable = isInternationalMerchant ?
          setShippingTypeForInternationalMerchant(shippingTypeEligibility) :
          setShippingTypeForNonInternationalMerchant(shippingTypeEligibility);

        for (String shippingOption : shippingOptionsAvailable) {
          CTRow row = sheetData.addNewRow();
          CTCell cell0 = row.insertNewC(PICKUP_POINTS_COLUMN1);
          cell0.setT(STCellType.STR);
          cell0.setV(shippingOption);
        }

        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
          OutputStream outputStream = sheetPart.getOutputStream()) {
          bos.write(worksheetDocument.xmlText().getBytes(StandardCharsets.UTF_8));
          outputStream.write(bos.toByteArray());
        }
      }
      try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
        opcpackage.save(bos);
        return bos.toByteArray();
      }
    }
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

  public static List<String> setShippingTypeForNonInternationalMerchant(
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

  public static void removeAttributeColumnsFromHeader(List<Object> excelBahasaHeaderList,
      List<Object> excelEnglishHeaderList, List<String> validEnglishHeaderList, List<String> validBahasaHeaderList,
      int startIndexOfAttributeColumn, int endIndexOfAttributeColumn) {
    List<String> englishAttributeHeaderList =
        new ArrayList<>(validEnglishHeaderList.subList(startIndexOfAttributeColumn, endIndexOfAttributeColumn));
    List<String> bahasaAttributeHeaderList =
        new ArrayList<>(validBahasaHeaderList.subList(startIndexOfAttributeColumn, endIndexOfAttributeColumn));
    List<Object> excelEnglishAttributeHeaderList =
        new ArrayList<>(excelEnglishHeaderList.subList(startIndexOfAttributeColumn, endIndexOfAttributeColumn));
    List<Object> excelBahasaAttributeHeaderList =
        new ArrayList<>(excelBahasaHeaderList.subList(startIndexOfAttributeColumn, endIndexOfAttributeColumn));
    excelEnglishHeaderList.removeAll(excelEnglishAttributeHeaderList);
    excelBahasaHeaderList.removeAll(excelBahasaAttributeHeaderList);
    validEnglishHeaderList.removeAll(englishAttributeHeaderList);
    validBahasaHeaderList.removeAll(bahasaAttributeHeaderList);
  }

  public static boolean isExcelHeaderInvalid(List<Object> excelBahasaHeaderList, List<Object> excelEnglishHeaderList,
      List<String> validEnglishHeaderList, List<String> validBahasaHeaderList) {
    return !(CollectionUtils.isEqualCollection(excelEnglishHeaderList, validEnglishHeaderList)
        || CollectionUtils.isEqualCollection(excelBahasaHeaderList, validBahasaHeaderList));
  }

  public static void setRowData(DownloadSkuResponse skuResponse, List<String> row) {
    row.add(Optional.ofNullable(skuResponse.getItemSku()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getPickupPointCode()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getPickupPointName()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getItemSkuName()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getStoreName()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getC1CategoryName()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getCnCategoryName()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getBrandName()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getProductType()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(String.valueOf(skuResponse.getListPrice()));
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(String.valueOf(skuResponse.getMinMargin()));
    row.add(String.valueOf(skuResponse.getMaxMargin()));
    row.add(Optional.ofNullable(skuResponse.getCogs()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getVatInclusive())
        .map(vatInclusive -> vatInclusive ? Constant.YES : Constant.NO)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getRebate()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    if (skuResponse.isRebateOverwrittenByOfficer()) {
      row.add(Constant.INPUT_BY_BR);
    } else {
      SkuRebateResponse rebateBreakdown = skuResponse.getRebateBreakdown();
      if (Objects.nonNull(rebateBreakdown)) {
        row.add(String.format(Constant.REBATE_HISTORY, rebateBreakdown.getSellerLevel(),
            rebateBreakdown.getSellerCategoryLevel(), rebateBreakdown.getSellerBrandLevel(),
            rebateBreakdown.getSellerCategoryBrandLevel()));
      } else {
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
      }
    }
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(String.valueOf(skuResponse.getOfferPrice()));
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(org.apache.commons.lang.StringUtils.EMPTY);
    row.add(Optional.ofNullable(skuResponse.getBmlPrice()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    CampaignPriceInfo priceInfo = skuResponse.getCampaignPriceInfo();
    if (Objects.nonNull(priceInfo)) {
      if (BooleanUtils.isTrue(priceInfo.isRegisteredInCampaign())) {
        row.add(Constant.YES);
        row.add(priceInfo.getCampaignCode());
        row.add(Optional.ofNullable(priceInfo.getCampaignName()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
        row.add(String.valueOf(priceInfo.getCampaignPrice()));
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(priceInfo.isLockPriceUpdate() ?
            Constant.SELLING_PRICE_CHANGE_NOT_ALLOWED :
            String.format(Constant.PRICE_RANGE, (long) priceInfo.getMinAllowedPrice(),
                (long) priceInfo.getMaxAllowedPrice()));
        row.add(priceInfo.isLockCampaignPriceUpdate() ?
            Constant.CAMPAIGN_PRICE_CHANGE_NOT_ALLOWED :
            String.format(Constant.PRICE_RANGE, (long) priceInfo.getMinCampaignPrice(),
                (long) priceInfo.getMaxCampaignPrice()));
      } else {
        row.add(Constant.NO);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
        row.add(org.apache.commons.lang.StringUtils.EMPTY);
      }
    } else {
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
      row.add(org.apache.commons.lang.StringUtils.EMPTY);
    }
    row.add(Optional.ofNullable(skuResponse.getTotalOrders()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getTotalTPV()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getL5Stock()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getExpiryDate()).orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getExpiringQuantity()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getL2Stock()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getL2DaysOfInventory()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
    row.add(Optional.ofNullable(skuResponse.getL2TargetDaysOfInventory()).map(String::valueOf)
        .orElse(org.apache.commons.lang.StringUtils.EMPTY));
  }
}
