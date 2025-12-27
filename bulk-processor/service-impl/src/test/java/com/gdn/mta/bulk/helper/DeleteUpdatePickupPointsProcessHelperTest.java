package com.gdn.mta.bulk.helper;


import java.util.Arrays;
import java.util.List;

import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.DeleteUpdatePickUpPointResponse;
import com.gdn.mta.bulk.models.download.responsedata.DeleteUpdatePickupPointDataResponse;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

public class DeleteUpdatePickupPointsProcessHelperTest {

  private static final String ITEM_SKU = "Item Sku";
  private static final String PICKUP_POINT = "Pickup point Code";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private DeleteUpdatePickupPointsProcessHelper deleteUpdatePickupPointsProcessHelper;

  private Workbook workbook;
  private DeleteUpdatePickupPointDataResponse deleteUpdatePickupPointDataResponse;
  private BulkDownloadRequest bulkDownloadRequest;
  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(ITEM_SKU).add(PICKUP_POINT).build();

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    DeleteUpdatePickUpPointResponse deleteUpdatePickUpPointResponse = new DeleteUpdatePickUpPointResponse();
    deleteUpdatePickupPointDataResponse = new DeleteUpdatePickupPointDataResponse();
    deleteUpdatePickUpPointResponse.setPickupPointCode(PICKUP_POINT);
    deleteUpdatePickUpPointResponse.setItemSku(ITEM_SKU);
    deleteUpdatePickupPointDataResponse.setDeleteUpdatePickUpPointResponseList(
        Arrays.asList(deleteUpdatePickUpPointResponse));
    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.DELETE_UPDATE_PICKUP_POINTS);
    bulkDownloadRequest.setRequestId(REQUEST_ID);
    workbook = new SXSSFWorkbook();
  }

  @Test
  public void getCsvHeadersMapTest() {
    Assertions.assertNull(deleteUpdatePickupPointsProcessHelper.getCsvHeadersMap(bulkDownloadRequest));
  }

  @Test
  public void getHeaderListTest() throws Exception {
    Assertions.assertEquals(HEADER_LIST,
        deleteUpdatePickupPointsProcessHelper.getHeaderList(deleteUpdatePickupPointDataResponse));
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    workbook.createSheet(PICKUP_POINT);
    List<DeleteUpdatePickUpPointResponse> responses =
        Arrays.asList(new DeleteUpdatePickUpPointResponse("SKU123", "PP123"),
            new DeleteUpdatePickUpPointResponse("SKU456", "PP456"));
    DeleteUpdatePickupPointDataResponse response = new DeleteUpdatePickupPointDataResponse(responses);
    deleteUpdatePickupPointsProcessHelper.modifyWorkbook(workbook, response);
  }

  @Test
  public void getRowDataTest() {
    List<List<String>> rowData = deleteUpdatePickupPointsProcessHelper.getRowData(deleteUpdatePickupPointDataResponse);
    Assertions.assertEquals(ITEM_SKU, rowData.get(0).get(0));
    Assertions.assertEquals(PICKUP_POINT, rowData.get(0).get(1));
  }

  @Test
  public void getDirectoryTest() {
    Assertions.assertEquals(ProcessorUtils.DELETE_UPDATE_PICKUP_POITS_TEMPLATE_DIR + REQUEST_ID,
        deleteUpdatePickupPointsProcessHelper.getDirectory(bulkDownloadRequest));
  }

  @Test
  public void getEmailParamsTest() {
    Assertions.assertNull(deleteUpdatePickupPointsProcessHelper.getEmailParams(bulkDownloadRequest, "eng"));
  }

  @Test
  public void getRecordsUpdatedTest() {
    Assertions.assertEquals(0,
        deleteUpdatePickupPointsProcessHelper.getRecordsUpdated(deleteUpdatePickupPointDataResponse));
  }
}