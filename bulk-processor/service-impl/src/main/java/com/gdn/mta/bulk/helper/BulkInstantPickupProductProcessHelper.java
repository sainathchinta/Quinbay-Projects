package com.gdn.mta.bulk.helper;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkInstantPickupProductResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.google.common.collect.ImmutableList;

@Component("bulkInstantPickupProductProcessHelper")
public class BulkInstantPickupProductProcessHelper extends BulkProcessHelper {

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.BLIBLI_SKU)
          .add(BulkParameters.PRODUCT_NAME_NON_EDITABLE).add(BulkParameters.PICKUP_POINT_CODE)
          .add(BulkParameters.PICKUP_POINT_NAME_NON_EDITABLE).add(BulkParameters.LIST_PRICE)
          .add(BulkParameters.OFFER_PRICE).add(BulkParameters.STOCK).build();

  private static final int PRODUCT_NAME_COLUMN_INDEX = 2;
  private static final int PICKUP_POINT_NAME_COLUMN_INDEX = 4;

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public byte[] generateCsvFile(BulkCsvModel csvModel, BulkDataResponse dataResponse)
      throws IllegalAccessException, NoSuchMethodException, InvocationTargetException, IOException {
    return super.generateCsvFile(csvModel, dataResponse);
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    return HEADER_LIST;
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkParameters.DATA_SHEET);
    Row row;
    row = dataSheet.createRow((short) 0);
    int cellIndex = 0;
    for (String headers : headerList) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(headers);
      CellStyle headerCellStyle = workbook.createCellStyle();
      headerCellStyle.setAlignment(HorizontalAlignment.CENTER);
      if (StringUtils.equals(cell.getStringCellValue(), BulkParameters.PRODUCT_NAME_NON_EDITABLE) ||
          StringUtils.equals(cell.getStringCellValue(), BulkParameters.PICKUP_POINT_NAME_NON_EDITABLE)) {
        Font headerFont = workbook.createFont();
        headerFont.setBold(true);
        addBackgroundColour(headerCellStyle, IndexedColors.GREY_40_PERCENT.getIndex());
        headerCellStyle.setFont(headerFont);
      }
      cell.setCellStyle(headerCellStyle);
    }
    CellStyle greyColorCellStyle = workbook.createCellStyle();
    int rowId = 1;
    for (List<String> rowData : rowDataList) {
      row = dataSheet.createRow(rowId);
      rowId++;
      int cellId = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellId);
        cellId++;
        if (cellId == PRODUCT_NAME_COLUMN_INDEX || cellId == PICKUP_POINT_NAME_COLUMN_INDEX) {
          addBackgroundColour(greyColorCellStyle, IndexedColors.GREY_40_PERCENT.getIndex());
          cell.setCellStyle(greyColorCellStyle);
        }
        cell.setCellValue(cellValue);
      }
    }
    return workbook;
  }

  @Override
  public void generateFile(String filepath, byte[] content) throws IOException {
    super.generateFile(filepath, content);
  }

  @Override
  public String getUserName(String email) {
    return super.getUserName(email);
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkInstantPickupProductResponse bulkInstantPickupProductResponse =
        (BulkInstantPickupProductResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for (OfflineItemInstantPickupBulkDownloadResponse offlineItemInstantPickupBulkDownloadResponse : bulkInstantPickupProductResponse
        .getOfflineItemInstantPickupResponses()) {
      List<String> rowDatum = new ArrayList<>();
      rowDatum.add(offlineItemInstantPickupBulkDownloadResponse.getItemSku());
      rowDatum.add(offlineItemInstantPickupBulkDownloadResponse.getItemName());
      rowDatum.add(offlineItemInstantPickupBulkDownloadResponse.getPickupPointCode());
      rowDatum.add(offlineItemInstantPickupBulkDownloadResponse.getPickupPointName());
      Double offerPrice = offlineItemInstantPickupBulkDownloadResponse.getPrice();
      Double listPrice =
          Optional.ofNullable(offlineItemInstantPickupBulkDownloadResponse.getListPrice())
              .orElse(offerPrice);
      rowDatum.add(ProcessorUtils.formatNumberByRemovingDenominator(listPrice));
      rowDatum.add(ProcessorUtils.formatNumberByRemovingDenominator(offerPrice));
      rowDatum.add(
          String.valueOf(offlineItemInstantPickupBulkDownloadResponse.getOfflineAvailableStock()));
      rowData.add(rowDatum);
    }
    return rowData;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.INSTANT_PICKUP_PRODUCT)
        + request.getRequestId();
  }

  @Override
  public String getFilePath(String directoryPath, String filename) {
    return super.getFilePath(directoryPath, filename);
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put("name", getUserName(request.getUsername()));
    emailParameters.put("reqId", request.getRequestId());
    emailParameters.put("businessPartnerCode", request.getMerchantId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, MessageUtil
        .getMessage(EmailConstants.BULK_INSTANT_PICKUP_PRODUCT_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM,
        EmailConstants.INSTANT_PICKUP_PRODUCT_SUBJECT);
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkInstantPickupProductResponse bulkInstantPickupProductResponse =
        (BulkInstantPickupProductResponse) response;
    return bulkInstantPickupProductResponse.getOfflineItemInstantPickupResponses().size();
  }

  private void addBackgroundColour(CellStyle backgroundStyle, short colourIndex) {
    backgroundStyle.setFillForegroundColor(colourIndex);
    backgroundStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
  }
}
