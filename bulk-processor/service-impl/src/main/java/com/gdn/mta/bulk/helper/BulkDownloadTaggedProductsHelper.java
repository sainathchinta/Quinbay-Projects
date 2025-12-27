package com.gdn.mta.bulk.helper;

import static com.gdn.mta.bulk.models.EmailConstants.BULK_TAGGING_PRODUCTS_DOWNLOAD_TEMPLATE_ID;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.response.BulkDownloadTaggedProductsResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

@Service
public class BulkDownloadTaggedProductsHelper extends BulkProcessHelper {

  private static final List<String> TAGGED_PRODUCTS_HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.SKU_ID).add(BulkParameters.PICKUP_POINT_CODE_FOR_TAGGING)
          .add(BulkParameters.CATEGORY_FOR_TAGGING).add(BulkParameters.STORE_NAME_FOR_TAGGING)
          .add(BulkParameters.PRODUCT_TYPE_FOR_TAGGING).add(BulkParameters.LAST_UPDATED).build();

  public static final String HEADER_FOR_TAGGING_PRODUCTS =
      "Use this sheet to update Pricing Product Types for your products";
  public static final String DATA_SHEET = "Data Sheet";
  public static final String PRODUCT_TYPE_TAGGING_FILE_IS_READY = "Product type tagging file is ready";
  public static final String DATE_FORMAT = "dd/MM/yy HH:mm";
  public static final String DELIMITER = " > ";

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return TAGGED_PRODUCTS_HEADER_LIST;
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) {
    XSSFWorkbook workbook = new XSSFWorkbook();
    XSSFSheet sheet = workbook.createSheet(DATA_SHEET);
    setDataSheet(headerList, rowDataList, sheet);
    return workbook;
  }

  private void setDataSheet(List<String> headerList, List<List<String>> rowDataList, XSSFSheet sheet) {
    Row headerRow = sheet.createRow(0);
    CellStyle yellowCellStyle = sheet.getWorkbook().createCellStyle();
    yellowCellStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
    yellowCellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    Font boldFont = sheet.getWorkbook().createFont();
    boldFont.setBold(true);
    yellowCellStyle.setFont(boldFont);
    CellStyle dataBorderStyle = sheet.getWorkbook().createCellStyle();
    borderStyle(yellowCellStyle);
    borderStyle(dataBorderStyle);
    for (int i = 0; i < headerList.size(); i++) {
      Cell cell = headerRow.createCell(i);
      cell.setCellValue(headerList.get(i));
      cell.setCellStyle(yellowCellStyle);
    }
    //data starts from row 1
    int rowNum = 1;
    for (List<String> rowData : rowDataList) {
      Row row = sheet.createRow(rowNum++);
      int start;
      for (start = 0; start < rowData.size(); start++) {
        Cell cell = row.createCell(start);
        cell.setCellValue(rowData.get(start));
        cell.setCellStyle(dataBorderStyle);
      }
    }
    for (int i = 0; i < headerList.size(); i++) {
      sheet.autoSizeColumn(i);
    }
  }

  private static void borderStyle(CellStyle dataBorderStyle) {
    dataBorderStyle.setBorderTop(BorderStyle.THIN);
    dataBorderStyle.setBorderBottom(BorderStyle.THIN);
    dataBorderStyle.setBorderLeft(BorderStyle.THIN);
    dataBorderStyle.setBorderRight(BorderStyle.THIN);
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkDownloadTaggedProductsResponse bulkDownloadTaggedProductsResponse =
        (BulkDownloadTaggedProductsResponse) response;
    List<TaggedProductFilterResponse> taggedProductFilterResponseList =
        bulkDownloadTaggedProductsResponse.getTaggedProductFilterResponseList();
    SimpleDateFormat dateFormat = new SimpleDateFormat(DATE_FORMAT);
    return taggedProductFilterResponseList.stream().map(taggedProduct -> {
      String parentCategories =
          Optional.ofNullable(taggedProduct.getParentCategoryNames()).map(names -> String.join(DELIMITER, names))
              .orElse(StringUtils.EMPTY);
      String formattedDate =
          Optional.ofNullable(taggedProduct.getUserUpdatedDate()).map(dateFormat::format).orElse(StringUtils.EMPTY);
      return Arrays.asList(taggedProduct.getItemSku(), taggedProduct.getPickupPointCode(), parentCategories,
          taggedProduct.getBusinessPartnerName(), taggedProduct.getProductTypeName(), formattedDate);
    }).collect(Collectors.toList());
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.BULK_DOWNLOAD_TAGGED_PRODUCTS) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, request.getUsername());
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, BULK_TAGGING_PRODUCTS_DOWNLOAD_TEMPLATE_ID);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, PRODUCT_TYPE_TAGGING_FILE_IS_READY);
    emailParameters.put(EmailConstants.FILE_PREFIX, request.getRequestId());
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    return 0;
  }
}
