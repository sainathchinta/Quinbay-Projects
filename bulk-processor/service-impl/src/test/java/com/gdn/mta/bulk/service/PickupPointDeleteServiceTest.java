package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.UUID;

import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.PickupPointDeleteProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;

public class PickupPointDeleteServiceTest {

  @InjectMocks
  private PickupPointDeleteServiceBean pickupPointDeleteServiceBean;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessArgumentCaptor;

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP_POINT = "pickup-point";
  private static final String NEW_PICKUP_POINT = "new-pickup-point";
  private static final String PRODUCT_SKU = "HIC-60001-00003";
  private static final String STORE_ID = "store-id";
  private static final String DELETE_PICKUP = "DeletePickupPoint";
  private static final String ITEM_SKU_1 = "HIC-60001-00003-00001";
  private static final String ITEM_SKU_2 = "HIC-60001-00003-00002";

  private static final String RANDOM_BULK_PROCESS_CODE = UUID.randomUUID().toString();

  private PickupPointDeleteProcessDTO pickupPointDeleteProcessDTO;
  private BulkProcess bulkProcess;
  private BulkUpdateEventModel bulkUpdateEventModel;
  private List<BulkProcessData> bulkProcessDataList;
  private List<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeResponseList;
  private List<ProductSkuPickupPointResponseV2> productSkuPickupPointResponseList;
  private ObjectMapper mapper;


  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    pickupPointDeleteProcessDTO = new PickupPointDeleteProcessDTO();
    pickupPointDeleteProcessDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointDeleteProcessDTO.setStoreId(STORE_ID);
    pickupPointDeleteProcessDTO.setPickupPointCode(PICKUP_POINT);

    mapper = new ObjectMapper();

    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(RANDOM_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(DELETE_PICKUP);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setNotes(PICKUP_POINT);
    bulkProcess.setCreatedBy("developer");
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStoreId("10001");
    bulkProcess.setUpdatedBy("developer");
    bulkProcess.setUpdatedDate(new Date());

    bulkUpdateEventModel = new BulkUpdateEventModel();
    bulkUpdateEventModel.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkUpdateEventModel.setStoreId(STORE_ID);
    bulkUpdateEventModel.setRowNumbers(Arrays.asList(1, 2));

    bulkProcessDataList = new ArrayList<>();
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setParentProduct(PRODUCT_SKU);
    bulkProcessData1.setRowNumber(1);
    bulkProcessData1.setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"state\":\"ACTIVE\",\"businessPartnerCode\":\"HIC-60001\"}");
    bulkProcessDataList.add(bulkProcessData1);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"state\":\"IN_PROGRESS\",\"businessPartnerCode\":\"HIC-60001\"}");
    bulkProcessData2.setRowNumber(2);
    bulkProcessData2.setParentProduct(PRODUCT_SKU);
    bulkProcessDataList.add(bulkProcessData2);

    inProgressProductsByPickupPointCodeResponseList = new ArrayList<>();
    InProgressProductsByPickupPointCodeResponse inProgressProductsByPickupPointCodeResponse1 =
        new InProgressProductsByPickupPointCodeResponse();
    inProgressProductsByPickupPointCodeResponse1.setProductSku(PRODUCT_SKU);
    inProgressProductsByPickupPointCodeResponse1.setPickupPointCode(PICKUP_POINT);
    inProgressProductsByPickupPointCodeResponse1.setItemSku(ITEM_SKU_1);
    InProgressProductsByPickupPointCodeResponse inProgressProductsByPickupPointCodeResponse2 =
        new InProgressProductsByPickupPointCodeResponse();
    inProgressProductsByPickupPointCodeResponse2.setProductSku(PRODUCT_SKU);
    inProgressProductsByPickupPointCodeResponse2.setPickupPointCode(PICKUP_POINT);
    inProgressProductsByPickupPointCodeResponse2.setItemSku(ITEM_SKU_2);
    inProgressProductsByPickupPointCodeResponseList.addAll(
        Arrays.asList(inProgressProductsByPickupPointCodeResponse1, inProgressProductsByPickupPointCodeResponse2));

    productSkuPickupPointResponseList = new ArrayList<>();
    ProductSkuPickupPointResponseV2 productSkuPickupPointResponse1 = new ProductSkuPickupPointResponseV2();
    productSkuPickupPointResponse1.setProductSku(PRODUCT_SKU);
    productSkuPickupPointResponse1.setPickupPointCode(PICKUP_POINT);
    productSkuPickupPointResponse1.setItemSku(ITEM_SKU_1);
    ProductSkuPickupPointResponseV2 productSkuPickupPointResponse2 = new ProductSkuPickupPointResponseV2();
    productSkuPickupPointResponse2.setProductSku(PRODUCT_SKU);
    productSkuPickupPointResponse2.setPickupPointCode(PICKUP_POINT);
    productSkuPickupPointResponse2.setItemSku(ITEM_SKU_2);
    productSkuPickupPointResponseList.addAll(
        Arrays.asList(productSkuPickupPointResponse1, productSkuPickupPointResponse2));
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "deletePickupPointErrorStatus",
        "IN_PROGRESS,PENDING,PROCESSED");
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(bulkUpdateServiceUtil);
  }

  @Test
  public void processDeletePickupPointEventTest() throws Exception {
    bulkProcess.setNotes(PICKUP_POINT);
    bulkProcess.setStatus("FINISHED");
    Mockito.when(bulkProcessService.findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(STORE_ID,
            BUSINESS_PARTNER_CODE, BulkProcessType.DELETE_PICKUP_POINT.getValue(), PICKUP_POINT))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(bulkUpdateServiceUtil
      .getBulkProcess(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),
        Mockito.any(BulkUpdateProcessDTO.class), Mockito.anyInt(),Mockito.anyInt(), anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
    this.pickupPointDeleteServiceBean.processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    Mockito.verify(bulkProcessService)
      .findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(STORE_ID, BUSINESS_PARTNER_CODE,
        BulkProcessType.DELETE_PICKUP_POINT.getValue(), PICKUP_POINT);
    Mockito.verify(bulkUpdateServiceUtil)
      .getBulkProcess(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),
        Mockito.any(BulkUpdateProcessDTO.class), Mockito.anyInt(),Mockito.anyInt(), anyBoolean(), anyBoolean());
    Mockito.verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
  }

  @Test
  public void processDeletePickupPointEventNullTest() throws Exception {
    bulkProcess.setNotes(PICKUP_POINT);
    Mockito.when(bulkProcessService.findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(STORE_ID,
            BUSINESS_PARTNER_CODE, BulkProcessType.DELETE_PICKUP_POINT.getValue(), PICKUP_POINT))
        .thenReturn(new ArrayList<>());
    Mockito.when(bulkUpdateServiceUtil.getBulkProcess(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(BulkUpdateProcessDTO.class), Mockito.anyInt(), Mockito.anyInt(), anyBoolean(), anyBoolean()))
        .thenReturn(bulkProcess);
    this.pickupPointDeleteServiceBean.processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    Mockito.verify(bulkProcessService)
        .findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(STORE_ID, BUSINESS_PARTNER_CODE,
            BulkProcessType.DELETE_PICKUP_POINT.getValue(), PICKUP_POINT);
    Mockito.verify(bulkUpdateServiceUtil).getBulkProcess(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(BulkUpdateProcessDTO.class), Mockito.anyInt(), Mockito.anyInt(), anyBoolean(), anyBoolean());
    Mockito.verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
  }

  @Test
  public void processDeletePickupPointEventExistingProcessTest() throws Exception {
    bulkProcess.setNotes(PICKUP_POINT);
    bulkProcess.setStatus("IN_PROGRESS");
    Mockito.when(bulkProcessService.findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(STORE_ID,
            BUSINESS_PARTNER_CODE, BulkProcessType.DELETE_PICKUP_POINT.getValue(), PICKUP_POINT))
        .thenReturn(List.of(bulkProcess));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.pickupPointDeleteServiceBean.processDeletePickupPointEvent(
              pickupPointDeleteProcessDTO));
    } finally {
      Mockito.verify(bulkProcessService)
          .findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(STORE_ID, BUSINESS_PARTNER_CODE,
              BulkProcessType.DELETE_PICKUP_POINT.getValue(), PICKUP_POINT);
    }
  }

  @Test
  public void processDeleteItemPickupPointEventTest() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
        new ItemSkuPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, BUSINESS_PARTNER_CODE,
            Arrays.asList("HIC-60001-00003-00001", "HIC-60001-00003-00002"));
    DeleteInProgressL5Response deleteInProgressL5Response =
        new DeleteInProgressL5Response("HIC-60001-00003-00002", PICKUP_POINT, "Not eligible to delete L5",
            NEW_PICKUP_POINT);
    bulkProcessDataList.get(1).setNotes(objectMapper.writeValueAsString(deleteInProgressL5Response));
    List<DeleteInProgressL5Response> deleteInProgressL5ResponseList = new ArrayList<>();
    deleteInProgressL5ResponseList.add(deleteInProgressL5Response);
    when(pbpOutboundService.deleteL5ByPickupPointCode(itemSkuPickupPointRequest)).thenReturn(
        deleteInProgressL5ResponseList);
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService, times(2)).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(pbpOutboundService).deleteL5ByPickupPointCode(itemSkuPickupPointRequest);
  }

  @Test
  public void processDeleteItemPickupPointEventPBPExceptionTest() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
        new ItemSkuPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, BUSINESS_PARTNER_CODE,
            Arrays.asList("HIC-60001-00003-00001", "HIC-60001-00003-00002"));
    DeleteInProgressL5Response deleteInProgressL5Response =
        new DeleteInProgressL5Response("HIC-60001-00003-00002", PICKUP_POINT, "Not eligible to delete L5",
            NEW_PICKUP_POINT);
    bulkProcessDataList.get(1).setNotes(objectMapper.writeValueAsString(deleteInProgressL5Response));
    List<DeleteInProgressL5Response> deleteInProgressL5ResponseList = new ArrayList<>();
    deleteInProgressL5ResponseList.add(deleteInProgressL5Response);
    Mockito.doThrow(ApplicationRuntimeException.class).when(pbpOutboundService)
        .deleteL5ByPickupPointCode(itemSkuPickupPointRequest);
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(4)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(pbpOutboundService).deleteL5ByPickupPointCode(itemSkuPickupPointRequest);
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Assertions.assertEquals(2, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
  }

  @Test
  public void processDeleteItemPickupPointFalseEventTest() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
        new ItemSkuPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, BUSINESS_PARTNER_CODE,
            Arrays.asList("HIC-60001-00003-00001", "HIC-60001-00003-00002"));
    DeleteInProgressL5Response deleteInProgressL5Response =
        new DeleteInProgressL5Response("HIC-60001-00003-00001", PICKUP_POINT, "", null);
    List<DeleteInProgressL5Response> deleteInProgressL5ResponseList = new ArrayList<>();
    deleteInProgressL5ResponseList.add(deleteInProgressL5Response);
    when(pbpOutboundService.deleteL5ByPickupPointCode(itemSkuPickupPointRequest)).thenReturn(
        deleteInProgressL5ResponseList);
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService, times(2)).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(pbpOutboundService).deleteL5ByPickupPointCode(itemSkuPickupPointRequest);
  }
  @Test
  public void processDeleteItemPickupPointEventSuccessTest() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
        new ItemSkuPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, BUSINESS_PARTNER_CODE,
            Arrays.asList("HIC-60001-00003-00002"));
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
  }

  @Test
  public void processDeleteItemPickupPointEventSuccessNoCallToPBPTest() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
        new ItemSkuPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, BUSINESS_PARTNER_CODE,
            Arrays.asList("HIC-60001-00003-00001", "HIC-60001-00003-00002"));
    when(pbpOutboundService.deleteL5ByPickupPointCode(itemSkuPickupPointRequest)).thenReturn(new ArrayList<>());
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(pbpOutboundService).deleteL5ByPickupPointCode(itemSkuPickupPointRequest);
  }

  @Test
  public void processDeleteItemPickupPointEventSuccessNoCallToPBPExceptionTest() throws Exception {
    bulkProcessDataList.get(1).setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"state\":\"ACTIVE\",\"businessPartnerCode\":\"HIC-60001\"}");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    DeleteItemPickupPointRequest deleteItemPickupPointRequest =
        new DeleteItemPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, Arrays.asList("HIC-60001-00003-00001","HIC-60001-00003-00002"),
            BUSINESS_PARTNER_CODE);
    when(xProductOutboundService.deleteItemPickupPointByPickupPointCode(deleteItemPickupPointRequest)).thenThrow(
        ApplicationRuntimeException.class);
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
  }

  @Test
  public void processDeleteItemPickupPointEventSuccessNoCallToPBPException1Test() throws Exception {
    bulkProcessDataList.get(1).setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"state\":\"ACTIVE\",\"businessPartnerCode\":\"HIC-60001\"}");
    bulkProcess.setSystemErrorCount(0);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    DeleteItemPickupPointRequest deleteItemPickupPointRequest =
        new DeleteItemPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, Arrays.asList("HIC-60001-00003-00001","HIC-60001-00003-00002"),
            BUSINESS_PARTNER_CODE);
    when(xProductOutboundService.deleteItemPickupPointByPickupPointCode(deleteItemPickupPointRequest)).thenThrow(
        ApplicationRuntimeException.class);
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
  }

  @Test
  public void processDeleteItemPickupPointEventSuccessNoCallToPBPException2Test() throws Exception {
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setParentProduct("HIC-60001-00003");
    bulkProcessData.setStatus("IN_PROGRESS");
    bulkProcessData.setBulkRequestData( "{\"itemSku\":\"HIC-60001-00003-00004\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"state\":\"IN_PROGRESS\",\"businessPartnerCode\":\"HIC-60001\"}");
    bulkProcessDataList.add(bulkProcessData);
    bulkProcessDataList.get(1).setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"state\":\"ACTIVE\",\"businessPartnerCode\":\"HIC-60001\"}");
    bulkProcess.setSystemErrorCount(1);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    DeleteItemPickupPointRequest deleteItemPickupPointRequest =
        new DeleteItemPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, Arrays.asList("HIC-60001-00003-00001","HIC-60001-00003-00002"),
            BUSINESS_PARTNER_CODE);
    when(xProductOutboundService.deleteItemPickupPointByPickupPointCode(deleteItemPickupPointRequest)).thenThrow(
        ApplicationRuntimeException.class);
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, Mockito.times(3)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
  }


  @Test
  public void processDeleteItemPickupPointEventSuccessNoCallToXproductTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"state\":\"IN_PROGRESS\",\"businessPartnerCode\":\"HIC-60001\"}");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0)).thenReturn(bulkProcessDataList.get(1));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
            mapper.readValue(bulkProcessDataList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(bulkProcessDataList.get(1).getBulkRequestData(), typeRef));
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
        new ItemSkuPickupPointRequest(PRODUCT_SKU, PICKUP_POINT, BUSINESS_PARTNER_CODE,
            Arrays.asList("HIC-60001-00003-00001","HIC-60001-00003-00002"));
    when(pbpOutboundService.deleteL5ByPickupPointCode(itemSkuPickupPointRequest)).thenReturn(new ArrayList<>());
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(pbpOutboundService).deleteL5ByPickupPointCode(itemSkuPickupPointRequest);
    verify(this.bulkProcessDataService, Mockito.times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(bulkProcessDataList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
  }


  @Test
  public void processDeleteItemPickupPointEventEmptyBulkProcessDataListTest() throws Exception
  {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        new ArrayList<>());
    pickupPointDeleteServiceBean.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void processPendingDeletePickupPointEventTest() throws Exception {
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchInProgressPickupPointSize", 1);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchActivePickupPointSize", 1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    systemParameterConfig.setValue(String.valueOf(1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
        bulkProcessService.findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1))).thenReturn(Collections.singletonList(bulkProcess));
    Page<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeResponsePage1 =
        new PageImpl<>(Arrays.asList(inProgressProductsByPickupPointCodeResponseList.get(0)), PageRequest.of(0, 1),
            inProgressProductsByPickupPointCodeResponseList.size());
    Page<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeResponsePage2 =
        new PageImpl<>(Arrays.asList(inProgressProductsByPickupPointCodeResponseList.get(1)), PageRequest.of(1, 1),
            inProgressProductsByPickupPointCodeResponseList.size());
    Page<ProductSkuPickupPointResponseV2> productSkuPickupPointResponsePage1 =
        new PageImpl<>(Arrays.asList(productSkuPickupPointResponseList.get(0)), PageRequest.of(0, 1),
            productSkuPickupPointResponseList.size());
    Page<ProductSkuPickupPointResponseV2> productSkuPickupPointResponsePage2 =
        new PageImpl<>(Arrays.asList(productSkuPickupPointResponseList.get(1)), PageRequest.of(1, 1),
            productSkuPickupPointResponseList.size());
    Mockito.when(pbpOutboundService.getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getNotes(), 0, 1)).thenReturn(inProgressProductsByPickupPointCodeResponsePage1);
    Mockito.when(pbpOutboundService.getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getNotes(), 1, 1)).thenReturn(inProgressProductsByPickupPointCodeResponsePage2);
    Mockito.when(xProductOutboundService.getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getNotes(), 0, 1)).thenReturn(productSkuPickupPointResponsePage1);
    Mockito.when(xProductOutboundService.getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getNotes(), 1, 1)).thenReturn(productSkuPickupPointResponsePage2);
    pickupPointDeleteServiceBean.processPendingDeletePickupPointEvent(STORE_ID,
        BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    Mockito.verify(bulkProcessService)
        .findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1));
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any());
    Mockito.verify(bulkProcessDataService).saveOperationBulkProcessData(Mockito.anyList());
    Mockito.verify(objectMapper, Mockito.times(4)).writeValueAsString(Mockito.any());
    Mockito.verify(pbpOutboundService)
        .getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 0, 1);
    Mockito.verify(pbpOutboundService)
        .getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 1, 1);
    Mockito.verify(xProductOutboundService)
        .getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 0, 1);
    Mockito.verify(xProductOutboundService)
        .getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 1, 1);
  }

  @Test
  public void processPendingDeletePickupPointEventWithPickedStatusSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchInProgressPickupPointSize", 1);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "updateProcessStatusAsPicked", true);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchActivePickupPointSize", 1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    systemParameterConfig.setValue(String.valueOf(1));
    Mockito.when(bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
      bulkProcessService.findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
        BulkProcess.STATUS_PENDING, PageRequest.of(0, 1))).thenReturn(Collections.singletonList(bulkProcess));
    Page<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeResponsePage1 =
      new PageImpl<>(Arrays.asList(inProgressProductsByPickupPointCodeResponseList.get(0)), PageRequest.of(0, 1),
        inProgressProductsByPickupPointCodeResponseList.size());
    Page<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeResponsePage2 =
      new PageImpl<>(Arrays.asList(inProgressProductsByPickupPointCodeResponseList.get(1)), PageRequest.of(1, 1),
        inProgressProductsByPickupPointCodeResponseList.size());
    Page<ProductSkuPickupPointResponseV2> productSkuPickupPointResponsePage1 =
      new PageImpl<>(Arrays.asList(productSkuPickupPointResponseList.get(0)), PageRequest.of(0, 1),
        productSkuPickupPointResponseList.size());
    Page<ProductSkuPickupPointResponseV2> productSkuPickupPointResponsePage2 =
      new PageImpl<>(Arrays.asList(productSkuPickupPointResponseList.get(1)), PageRequest.of(1, 1),
        productSkuPickupPointResponseList.size());
    Mockito.when(pbpOutboundService.getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
      bulkProcess.getNotes(), 0, 1)).thenReturn(inProgressProductsByPickupPointCodeResponsePage1);
    Mockito.when(pbpOutboundService.getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
      bulkProcess.getNotes(), 1, 1)).thenReturn(inProgressProductsByPickupPointCodeResponsePage2);
    Mockito.when(xProductOutboundService.getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
      bulkProcess.getNotes(), 0, 1)).thenReturn(productSkuPickupPointResponsePage1);
    Mockito.when(xProductOutboundService.getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
      bulkProcess.getNotes(), 1, 1)).thenReturn(productSkuPickupPointResponsePage2);
    pickupPointDeleteServiceBean.processPendingDeletePickupPointEvent(STORE_ID,
      BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    Mockito.verify(bulkProcessService)
      .findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
        BulkProcess.STATUS_PENDING, PageRequest.of(0, 1));
    Mockito.verify(bulkProcessService, times(2)).saveOperation(Mockito.any());
    Mockito.verify(bulkProcessDataService).saveOperationBulkProcessData(Mockito.anyList());
    Mockito.verify(objectMapper, Mockito.times(4)).writeValueAsString(Mockito.any());
    Mockito.verify(pbpOutboundService)
      .getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 0, 1);
    Mockito.verify(pbpOutboundService)
      .getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 1, 1);
    Mockito.verify(xProductOutboundService)
      .getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 0, 1);
    Mockito.verify(xProductOutboundService)
      .getActiveProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 1, 1);
  }


  @Test
  public void processPendingDeletePickupPointEventExceptionTest() throws Exception
  {
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchInProgressPickupPointSize", 1);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchActivePickupPointSize", 1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    systemParameterConfig.setValue(String.valueOf(1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
        bulkProcessService.findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1))).thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(pbpOutboundService.getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getNotes(), 0, 1)).thenThrow(Exception.class);
    pickupPointDeleteServiceBean.processPendingDeletePickupPointEvent(STORE_ID,
        BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    Mockito.verify(bulkProcessService)
        .findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1));
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any());
    Mockito.verify(pbpOutboundService)
        .getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 0, 1);
  }

  @Test
  public void processPendingDeletePickupPointEventException1Test() throws Exception
  {
    bulkProcess.setInputErrorCount(1);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchInProgressPickupPointSize", 1);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchActivePickupPointSize", 1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    systemParameterConfig.setValue(String.valueOf(1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
        bulkProcessService.findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1))).thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(pbpOutboundService.getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getNotes(), 0, 1)).thenThrow(Exception.class);
    pickupPointDeleteServiceBean.processPendingDeletePickupPointEvent(STORE_ID,
        BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    Mockito.verify(bulkProcessService)
        .findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1));
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any());
    Mockito.verify(pbpOutboundService)
        .getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 0, 1);
  }

  @Test
  public void processPendingDeletePickupPointEventException2Test() throws Exception
  {
    bulkProcess.setInputErrorCount(0);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchInProgressPickupPointSize", 1);
    ReflectionTestUtils.setField(pickupPointDeleteServiceBean, "fetchActivePickupPointSize", 1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    systemParameterConfig.setValue(String.valueOf(1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
        bulkProcessService.findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1))).thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(pbpOutboundService.getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getNotes(), 0, 1)).thenThrow(Exception.class);
    pickupPointDeleteServiceBean.processPendingDeletePickupPointEvent(STORE_ID,
        BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE);
    Mockito.verify(bulkProcessService)
        .findByBulkProcessTypeAndStatus(STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),
            BulkProcess.STATUS_PENDING, PageRequest.of(0, 1));
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any());
    Mockito.verify(pbpOutboundService)
        .getInProgressProductsByPickupPointCode(bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes(), 0, 1);
  }


}
