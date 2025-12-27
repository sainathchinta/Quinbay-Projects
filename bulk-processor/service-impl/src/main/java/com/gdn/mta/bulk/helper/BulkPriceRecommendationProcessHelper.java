package com.gdn.mta.bulk.helper;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.gdn.mta.bulk.service.FileStorageOperationsService;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.response.BulkPriceAnalyticsResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;

@Service
public class BulkPriceRecommendationProcessHelper extends BulkProcessHelper{
  private static final Logger log = LoggerFactory.getLogger(BulkPriceRecommendationProcessHelper.class);
  private static final Object PRICING_INFORMATION_FILE_IS_READY = "Pricing information file is ready";
  private static final Object BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_ID =
      "BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_ID";
  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return BulkParameters.PRICE_RECOMMENDATION_HEADERS;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkPriceAnalyticsResponse bulkPriceAnalyticsResponse = (BulkPriceAnalyticsResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for (DownloadSkuResponse skuResponse : bulkPriceAnalyticsResponse.getDownloadSkuResponseList()) {
      List<String> row = new ArrayList<>();
      ExcelTemplateUtil.setRowData(skuResponse, row);
      rowData.add(row);
    }
    return rowData;
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) throws IOException {
    InputStream inputStream = new ByteArrayInputStream(fileStorageOperationsService.downloadBaseTemplateForPriceRecommendation());
    XSSFWorkbook workbook = new XSSFWorkbook(inputStream);
    XSSFSheet dataSheet = workbook.getSheet("Sheet1");
    Map<Integer, String> columnToNameMap = new HashMap<>();
    int rowId = 6;
    Row row;
    int cellIndex = 0;
    for (String headers : headerList) {
      cellIndex++;
      columnToNameMap.put(cellIndex, headers);
    }
    CellStyle percentageStyle = workbook.createCellStyle();
    percentageStyle.setDataFormat(workbook.createDataFormat().getFormat("0.00%"));
    percentageStyle.setAlignment(HorizontalAlignment.CENTER);
    percentageStyle.setVerticalAlignment(VerticalAlignment.CENTER);
    for (List<String> rowData : rowDataList) {
      row = dataSheet.getRow(rowId);
      rowId++;
      cellIndex = 0;
        for (String cellValue : rowData) {
          Cell cell = row.getCell(cellIndex);
          cellIndex++;
          String columnName = columnToNameMap.get(cellIndex);
          switch (columnName) {
            case BulkParameters.PRICE_COMPLIANT:
            case BulkParameters.BML:
            case BulkParameters.CURRENT_MARGIN:
            case BulkParameters.COGS_REBATE: {
              break;
            }
            case BulkParameters.NORMAL_PRICE:
            case BulkParameters.COGS_SKU:
            case BulkParameters.REBATE_SKU:
            case BulkParameters.SELLING_PRICE:
            case BulkParameters.CAMPAIGN_PRICE:
            case BulkParameters.BML_BEST_PRICE:
            case BulkParameters.TOTAL_ORDERS:
            case BulkParameters.TOTAL_TPV:
            case BulkParameters.L5_STOCK:
            case BulkParameters.L2_STOCK:
            case BulkParameters.L2_DAYS_OF_INVENTORY:
            case BulkParameters.EXPIRING_STOCK:
            case BulkParameters.L2_TARGET_DAYS_OF_INVENTORY: {
              if (StringUtils.isNotBlank(cellValue)) {
                double numericValue = Double.parseDouble(cellValue);
                cell.setCellValue(numericValue);
              }
              break;
            }
            case BulkParameters.MIN_DESIRED_MARGIN:
            case BulkParameters.MAX_DESIRED_MARGIN: {
              double numericValue = Double.parseDouble(cellValue) / 100.0;
              cell.setCellValue(numericValue);
              cell.setCellStyle(percentageStyle);
              break;
            }
            default:
              cell.setCellValue(cellValue);
              break;
          }
        }
      }
    FormulaEvaluator mainWorkbookEvaluator = workbook.getCreationHelper().createFormulaEvaluator();
    mainWorkbookEvaluator.evaluateAll();
    return workbook;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.BULK_PRICE_RECOMMENDATION) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
    emailParameters.put(EmailConstants.NAME, request.getUsername());
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_ID);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, PRICING_INFORMATION_FILE_IS_READY);
    emailParameters.put(EmailConstants.DATE, dateFormat.format(new Date()));
    request.setEmailTo(request.getUsername());
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    if (Objects.nonNull(response) && response instanceof BulkPriceAnalyticsResponse) {
      BulkPriceAnalyticsResponse bulkPriceAnalyticsResponse = (BulkPriceAnalyticsResponse) response;
      return Optional.ofNullable(bulkPriceAnalyticsResponse.getDownloadSkuResponseList()).map(List::size).orElse(0);
    }
    return 0;
  }
}
