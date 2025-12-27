package com.gdn.mta.bulk.helper;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.PickupPointModel;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyUploadTemplateResponse;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.ImmutableList;

@Component(value = "bulkStoreCopyTemplateProcessHelper")
public class BulkStoreCopyTemplateProcessHelper extends BulkProcessHelper{

  public static final String TOKO = "Toko";
  public static final String SHIPPING_TYPE = "Shipping Type";
  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.PICKUP_POINT_HEADER).build();

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    return new ArrayList<>(HEADER_LIST);
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    List<String> shippingList = ExcelTemplateUtil.setShippingTypeForNonInternationalMerchant(
      response.getShippingTypeEligibility());
    Sheet shippingType = workbook.getSheet(SHIPPING_TYPE);
    int rowIndex = 0;
    Row row;
    for (String shippingOption : shippingList) {
      row = shippingType.createRow(rowIndex);
      rowIndex++;
      Cell cellValue = row.createCell(0);
      cellValue.setCellValue(shippingOption);
    }
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    StoreCopyUploadTemplateResponse storeCopyUploadTemplateResponse = (StoreCopyUploadTemplateResponse) response;
    List<List<String>> pickupPoints = new ArrayList<>();
    for (PickupPointModel pickupPoint : storeCopyUploadTemplateResponse.getPickupPoints()) {
      List<String> pickupPointData = new ArrayList<>();
      pickupPointData.add(pickupPoint.getCode());
      pickupPointData.add(pickupPoint.getName());
      pickupPoints.add(pickupPointData);
    }
    return pickupPoints;
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth)
    throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(StringUtils.EMPTY);
    bulkProcess.setBulkProcessType(BulkProcessType.STORE_COPY_TEMPLATE.getValue());
    byte[] data =
      fileStorageOperationsService.downloadFile(bulkProcess, Constant.STORE_COPY_UPLOAD_BASE_TEMPLATE_XLSX);
    try (InputStream fileInputStream = new ByteArrayInputStream((data))) {
      XSSFWorkbook xssfWorkbook = new XSSFWorkbook(fileInputStream);
      XSSFSheet dataSheet = xssfWorkbook.getSheet(TOKO);
      int rowId = 0;
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
      bulkDownloadServiceBeanUtil.generateValidationForWorkbook(xssfWorkbook, rowDataList.size(),
          BulkParameters.PICKUP_POINT_DEFAULT_COLUMN_INDEX_IN_COPY_STORE, true, TOKO);
      return xssfWorkbook;
    } catch (IOException e) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, e.getMessage(), e);
    }
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE)
        + request.getMerchantId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    return null;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    return 0;
  }
}
