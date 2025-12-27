package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.DeleteUpdatePickUpPointResponse;
import com.gdn.mta.bulk.models.download.responsedata.DeleteUpdatePickupPointDataResponse;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

@Service(value = "deleteUpdatePickupPointsProcessHelper")
public class DeleteUpdatePickupPointsProcessHelper extends BulkProcessHelper {
  private static final String ITEM_SKU = "Item Sku";
  private static final String PICKUP_POINT = "Pickup point Code";
  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(ITEM_SKU).add(PICKUP_POINT).build();

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return HEADER_LIST;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    Sheet pickupPointSheet = workbook.createSheet("DELETE_UPDATE_PICKUP_POINT_SHEET");
    ((SXSSFSheet) pickupPointSheet).trackAllColumnsForAutoSizing();
    Row row = null;
    int cellIndex = 0;
    int rowindex = 1;
    DeleteUpdatePickupPointDataResponse deleteUpdatePickupPointDataResponse =
        (DeleteUpdatePickupPointDataResponse) response;
    List<String> headerList = DeleteUpdatePickupPointsProcessHelper.HEADER_LIST;
    row = pickupPointSheet.createRow((short) 0);
    for (String header : headerList) {
      Cell cell = row.createCell((short) cellIndex++);
      cell.setCellType(CellType.STRING);
      cell.setCellValue(header);
    }

    for (DeleteUpdatePickUpPointResponse deleteUpdatePickUpPointResponse : deleteUpdatePickupPointDataResponse.getDeleteUpdatePickUpPointResponseList()) {
      cellIndex = 0;
      row = pickupPointSheet.createRow((short) rowindex++);

      Cell cell1 = row.createCell((short) cellIndex++);
      cell1.setCellType(CellType.STRING);
      cell1.setCellValue(deleteUpdatePickUpPointResponse.getItemSku());

      Cell cell2 = row.createCell((short) cellIndex++);
      cell2.setCellType(CellType.STRING);
      cell2.setCellValue(deleteUpdatePickUpPointResponse.getPickupPointCode());
    }
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    DeleteUpdatePickupPointDataResponse deleteUpdatePickupPointDataResponse =
        (DeleteUpdatePickupPointDataResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for (DeleteUpdatePickUpPointResponse deleteUpdatePickUpPointResponse : deleteUpdatePickupPointDataResponse.getDeleteUpdatePickUpPointResponseList()) {
      List<String> row = Arrays.asList(deleteUpdatePickUpPointResponse.getItemSku(),
          deleteUpdatePickUpPointResponse.getPickupPointCode());
      rowData.add(row);
    }
    return rowData;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.DELETE_UPDATE_PICKUP_POINTS) + request.getRequestId();
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
