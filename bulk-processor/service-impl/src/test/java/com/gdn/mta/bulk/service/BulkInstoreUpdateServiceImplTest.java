package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.partners.bulk.util.Constant;

public class BulkInstoreUpdateServiceImplTest {
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BULK_PROCESS_CODE = "defaultBulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "TOQ-15120";

  @InjectMocks
  private BulkInstoreUpdateServiceImpl bulkInstoreUpdateService;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Captor
  private ArgumentCaptor<List<BulkProcessData>> bulkProcessDataArgumentCaptor;

  @BeforeEach
  public void setUp() {
    initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessService, bulkProcessDataService, systemParameterConfigService);
  }

  @Test
  public void processInstoreUpdateEventTest() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel =
        new BulkUpdateEventModel(DEFAULT_STORE_ID, BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2, 3, 4));
    BulkProcess bulkProcess  = new BulkProcess();

    when(bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING))
        .thenReturn(getBulkProcessDataForInstoreUpdate());
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE, "2",
            SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE));
    when(bulkProcessService
        .bulkUpdateOff2On(Mockito.anyMap(), eq(bulkProcess.getRequestId()),
            eq(bulkProcess.getCreatedBy()))).thenReturn(Arrays.asList("productSku2"));
    doNothing().when(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    when(bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataArgumentCaptor.capture()))
        .thenReturn(getBulkProcessDataForInstoreUpdate());

    bulkInstoreUpdateService.processInstoreUpdateEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    verify(bulkProcessService)
        .bulkUpdateOff2On(Mockito.anyMap(), eq(bulkProcess.getRequestId()),
            eq(bulkProcess.getCreatedBy()));
    verify(bulkProcessDataService).saveAndReturnBulkProcessData(bulkProcessDataArgumentCaptor.getAllValues().get(0));
    verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.getAllValues().get(1));

    Assertions.assertEquals(3, bulkProcessDataArgumentCaptor.getValue().stream()
        .filter(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus())).count());
    Assertions.assertEquals(1, bulkProcessDataArgumentCaptor.getValue().stream()
        .filter(bulkProcessData -> BulkProcessData.STATUS_SUCCESS.equals(bulkProcessData.getStatus())).count());
  }

  @Test
  public void processInstoreUpdateEventAllFailedTest() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel =
        new BulkUpdateEventModel(DEFAULT_STORE_ID, BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2, 3, 4));
    BulkProcessData bulkProcessDataWrongJson = new BulkProcessData();
    bulkProcessDataWrongJson.setBulkRequestData("");
    List<BulkProcessData> bulkProcessDatas = new ArrayList<>(getBulkProcessDataForInstoreUpdate());
    bulkProcessDatas.remove(0);
    bulkProcessDatas.remove(0);
    bulkProcessDatas.add(bulkProcessDataWrongJson);

    when(bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(new BulkProcess());
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDatas);
    doNothing().when(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    when(bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataArgumentCaptor.capture()))
        .thenReturn(bulkProcessDatas);

    bulkInstoreUpdateService.processInstoreUpdateEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService).saveAndReturnBulkProcessData(bulkProcessDataArgumentCaptor.getAllValues().get(0));
    verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.getAllValues().get(1));

    Assertions.assertEquals(3, bulkProcessDataArgumentCaptor.getValue().stream()
        .filter(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus())).count());
  }

  @Test
  public void processInstoreUpdateEventEmptyDataTest() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel =
        new BulkUpdateEventModel(DEFAULT_STORE_ID, BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2, 3, 4));

    when(bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(new BulkProcess());
    when(bulkProcessDataService
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING)).thenReturn(new ArrayList<>());

    bulkInstoreUpdateService.processInstoreUpdateEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);

  }

  @Test
  public void processInstoreUpdateEventExceptionTest() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel =
        new BulkUpdateEventModel(DEFAULT_STORE_ID, BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2, 3, 4));

    when(bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(new BulkProcess());
    when(bulkProcessDataService
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING)).thenReturn(getBulkProcessDataForInstoreUpdate());
    when(bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataArgumentCaptor.capture()))
        .thenThrow(ApplicationRuntimeException.class);
    doNothing().when(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());

    bulkInstoreUpdateService.processInstoreUpdateEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService).saveAndReturnBulkProcessData(bulkProcessDataArgumentCaptor.getAllValues().get(0));
    verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.getAllValues().get(1));
  }


  private List<BulkProcessData> getBulkProcessDataForInstoreUpdate() {
    String json1 = "{\"productSku\":\"productSku1\",\"off2OnFlag\":\"1.0\"}";
    String json2 = "{\"productSku\":\"productSku2\",\"off2OnFlag\":\"0.0\"}";
    String json3 = "{\"off2OnFlag\":\"1\"}";
    String json4 = "{\"productSku\":\"productSku3\",\"off2OnFlag\":\"3\"}";
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setSystemErrorCount(1);
    bulkProcessData2.setRowNumber(0);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);
    BulkProcessData bulkProcessData4 = new BulkProcessData();
    bulkProcessData3.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData3.setInputErrorCount(1);
    bulkProcessData3.setRowNumber(1);
    bulkProcessData4.setBulkRequestData(json4);
    return Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3, bulkProcessData4);
  }


}
