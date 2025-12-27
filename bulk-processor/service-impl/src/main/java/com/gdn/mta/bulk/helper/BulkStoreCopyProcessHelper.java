package com.gdn.mta.bulk.helper;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.StoreCopyDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyProductResponse;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.ImmutableList;

@Component(value = "bulkStoreCopyProcessHelper")
public class BulkStoreCopyProcessHelper extends BulkProcessHelper {

  public static final String DATA = "Data";
  @Value("${storeCopy.template.path}")
  private String storeCopyDownloadTemplatePath;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;


  // Can be used for validation later
  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.BLIBLI_PRODUCT_SKU)
          .add(BulkParameters.PARENT_PRODUCT_NAME).add(BulkParameters.BLIBLI_SKU)
          .add(BulkParameters.NAMA_PRODUK).add(BulkParameters.SKU_CODE_HEADER)
          .add(BulkParameters.PRODUCT_CODE).add(BulkParameters.COPY_PRODUCT_NAME)
          .add(BulkParameters.SELLER_SKU).add(BulkParameters.PRICE_HEADER)
          .add(BulkParameters.SELLING_PRICE_HEADER).add(BulkParameters.STOCK)
          .add(BulkParameters.PICKUP_POINT_HEADER)
          .add(BulkParameters.MINIMUM_STOCK_HEADER).add(BulkParameters.AMPHI_SKU_STATUS)
          .add(BulkParameters.SHIPPING_TYPE_HEADER).build();

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    List<String> headerList = new ArrayList<>(HEADER_LIST);
    return headerList;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    StoreCopyProductResponse storeCopyProductResponse = (StoreCopyProductResponse) response;
    return bulkDownloadServiceBeanUtil.getAllProductsDetailForStoreCopy(
        storeCopyProductResponse.getProductLevel3SummaryResponses());
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    StoreCopyDownloadRequest storeCopyDownloadRequest =
        (StoreCopyDownloadRequest) request;
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.STORE_COPY_PRODUCTS)
        + storeCopyDownloadRequest.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        MessageUtil.getMessage(EmailConstants.STORE_COPY_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.SELLER_CODE, request.getMerchantId());
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM,
        EmailConstants.STORE_COPY_DOWNLOAD_SUBJECT.replace(EmailConstants.SELLER_CODE, request.getMerchantId()));
    emailParameters.put(EmailConstants.FILE_PREFIX,
        fileStorageOperationsService.getFilePrefix(BulkProcessType.STORE_COPY.getValue()));
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    StoreCopyProductResponse storeCopyProductResponse = (StoreCopyProductResponse) response;
    return storeCopyProductResponse.getProductLevel3SummaryResponses().size();
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth)
    throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(StringUtils.EMPTY);
    bulkProcess.setBulkProcessType(BulkProcessType.STORE_COPY_TEMPLATE.getValue());
    byte[] data =
      fileStorageOperationsService.downloadFile(bulkProcess, Constant.STORE_COPY_DOWNLOAD_BASE_TEMPLATE_XLSX);
    try (InputStream fileInputStream = new ByteArrayInputStream((data))) {
      XSSFWorkbook xssfWorkbook = new XSSFWorkbook(fileInputStream);
      XSSFSheet dataSheet = xssfWorkbook.getSheet(DATA);
      int rowId = 1;
      Row row;
      for (List<String> rowData : rowDataList) {
        row = dataSheet.createRow(rowId);
        rowId++;
        int cellId = 0;
        for (String cellValue : rowData) {
          Cell cell = row.createCell(cellId);
          cell.setCellValue(Optional.ofNullable(cellValue).orElse(StringUtils.EMPTY));
          cellId++;
        }
      }
      return xssfWorkbook;
    } catch (IOException e) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, e.getMessage(), e);
    }
  }
}
