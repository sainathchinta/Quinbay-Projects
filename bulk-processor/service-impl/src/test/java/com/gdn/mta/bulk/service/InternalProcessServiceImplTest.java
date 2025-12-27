package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.models.download.UpdateRemoveProductTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.FailedReasonResponse;
import com.gdn.mta.bulk.util.RequestHelper;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.dto.BulkInternalProcessPendingDataDTO;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.repository.BulkInternalProcessDataRepository;
import com.gdn.mta.bulk.repository.BulkInternalProcessRepository;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.ImmutableSet;
import org.springframework.test.util.ReflectionTestUtils;

public class InternalProcessServiceImplTest {

  private static final int BATCH_SIZE = 10;
  private static final int PAGE = 0;
  public static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "product-code";
  public static final String INTERNAL_PROCESS = "internal-process";
  public static final String INTERNAL_ID = "internal-id";
  public static final String STATUS = "IN_PROGRESS";
  private static final String INTERNAL_PROCESS_REQUEST_CODE = "request-code";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "request-id";
  private static final String FILE_NAME = "Copy_product_template.xlsx";
  public static final String SELLER_CODE = "seller-code";
  private static final String PRODUCT_CODE1 = "product-code1";
  private static final String USER_NAME = "user-name";
  private static final String CANCELLED_BY_USER = "Cancelled by User - ";
  private static final String NOTES = "consignment-id";
  private static final String PARENT_CODE = "parent_code";

  private static final String ITEM_SKU = "item_sku";
  private static final String PICKUP_POINT_CODE = "pickup_point_code";


  private Pageable pageable = PageRequest.of(PAGE, BATCH_SIZE);
  private BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
  private BulkInternalProcessPendingDataDTO bulkInternalProcessPendingDataDTO;

  @InjectMocks
  private InternalProcessServiceImpl internalProcessServiceImpl;

  @Mock
  private BulkInternalProcessRepository bulkInternalProcessRepository;

  @Mock
  private BulkInternalProcessDataRepository bulkInternalProcessDataRepository;

  @Mock
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;



  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);

    bulkInternalProcess.setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcess.setSellerCode(SELLER_CODE);
    bulkInternalProcess.setFileName(FILE_NAME);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);

    bulkInternalProcessPendingDataDTO =
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(INTERNAL_ID)
            .processType(ProcessStatus.PENDING.name()).parentCode(PRODUCT_CODE).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkInternalProcessRepository);
    Mockito.verifyNoMoreInteractions(bulkInternalProcessDataRepository,
      priceAnalyticsOutboundService);
  }

  @Test
  public void getAllBulkInternalProcessByStatusTest() {
    internalProcessServiceImpl.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(bulkInternalProcessRepository)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID, ProcessStatus.PENDING.name(),
            BulkInternalProcessType.STORE_COPY.name(), pageable);
  }

  @Test
  public void saveInternalProcessTest() {
    internalProcessServiceImpl.saveInternalProcess(new BulkInternalProcess());
    Mockito.verify(bulkInternalProcessRepository).saveAndFlush(Mockito.any(BulkInternalProcess.class));
  }

  @Test
  public void saveInternalProcessDataTest() {
    internalProcessServiceImpl.saveInternalProcessData(Arrays.asList(new BulkInternalProcessData()));
    Mockito.verify(bulkInternalProcessDataRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void bulkInternalProcessSummaryTest() {
    BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest = new BulkInternalProcessSummaryRequest();
    bulkInternalProcessSummaryRequest.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    internalProcessServiceImpl.bulkInternalProcessSummary(STORE_ID, new BulkInternalProcessSummaryRequest(), PAGE, BATCH_SIZE);
    Mockito.verify(bulkInternalProcessRepository)
        .bulkInternalProcessSummary(STORE_ID, new BulkInternalProcessSummaryRequest(), PageRequest.of(PAGE, BATCH_SIZE));
  }

  @Test
  public void bulkInternalProcessSummarySalesCategoryTest() {
    BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest = new BulkInternalProcessSummaryRequest();
    bulkInternalProcessSummaryRequest.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    internalProcessServiceImpl.bulkInternalProcessSummary(STORE_ID, bulkInternalProcessSummaryRequest, PAGE, BATCH_SIZE);
    Mockito.verify(bulkInternalProcessRepository)
        .bulkInternalProcessSummary(STORE_ID, bulkInternalProcessSummaryRequest, PageRequest.of(PAGE, BATCH_SIZE));
  }

  @Test
  public void getAllBulkInternalProcessDataByStatusAndBatchSizeEmptyValueTest() {
    Mockito.when(bulkInternalProcessDataRepository
        .getDistinctParentCodeByStatusAndProcessType(STORE_ID, ProcessStatus.PENDING.name(),
            BulkInternalProcessType.STORE_COPY.name(), Arrays.asList(bulkInternalProcess.getId()), BATCH_SIZE))
        .thenReturn(Arrays.asList());
    internalProcessServiceImpl
        .getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(), BATCH_SIZE,
            BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess));
    Mockito.verify(bulkInternalProcessDataRepository)
        .getDistinctParentCodeByStatusAndProcessType(STORE_ID, ProcessStatus.PENDING.name(),
            BulkInternalProcessType.STORE_COPY.name(), Arrays.asList(bulkInternalProcess.getId()), BATCH_SIZE);
  }

  @Test
  public void getAllBulkInternalProcessDataByStatusAndBatchSizeTest() {
    Object[] bulkInternalProcessObject = {new String("id"), new String("pending"), new String("test")};
    List<Object[]> bulkInternalProcessPendingDataDTOList1 =
        Arrays.asList(bulkInternalProcessObject, bulkInternalProcessObject);
    List<BulkInternalProcessPendingDataDTO> bulkInternalProcessPendingDataDTOList = new ArrayList<>();
    bulkInternalProcessPendingDataDTOList.add(bulkInternalProcessPendingDataDTO);
    bulkInternalProcessPendingDataDTO.setParentCode(PRODUCT_CODE1);
    bulkInternalProcessPendingDataDTOList.add(bulkInternalProcessPendingDataDTO);
    Mockito.when(bulkInternalProcessDataRepository.getDistinctParentCodeByStatusAndProcessType(STORE_ID,
        ProcessStatus.PENDING.name(), BulkInternalProcessType.STORE_COPY.name(),
        Arrays.asList(bulkInternalProcess.getId()), BATCH_SIZE)).thenReturn(bulkInternalProcessPendingDataDTOList1);
    internalProcessServiceImpl.getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(),
        BATCH_SIZE, BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess));
    Mockito.verify(bulkInternalProcessDataRepository)
        .getDistinctParentCodeByStatusAndProcessType(STORE_ID, ProcessStatus.PENDING.name(),
            BulkInternalProcessType.STORE_COPY.name(), Arrays.asList(bulkInternalProcess.getId()), BATCH_SIZE);
  }

  @Test
  public void getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestIdTest() {
    internalProcessServiceImpl
        .getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(PRODUCT_CODE, INTERNAL_PROCESS,
            INTERNAL_ID);
    Mockito.verify(bulkInternalProcessDataRepository)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndAndMarkForDeleteFalse(PRODUCT_CODE,
            INTERNAL_PROCESS, INTERNAL_ID);
  }

  @Test
  public void findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalseTest() {
    internalProcessServiceImpl.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
        PRODUCT_CODE, INTERNAL_PROCESS, INTERNAL_ID, STATUS);
    Mockito.verify(bulkInternalProcessDataRepository)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            INTERNAL_PROCESS, INTERNAL_ID, STATUS);
  }

  @Test
  public void saveInternalProcessesTest() {
    BulkInternalProcess bulkInternalProcess = BulkInternalProcess.builder().build();
    internalProcessServiceImpl.saveInternalProcesses(Arrays.asList(bulkInternalProcess));
    Mockito.verify(bulkInternalProcessRepository).saveAll(Arrays.asList(bulkInternalProcess));
  }

  @Test
  public void getCountByStoreIdAndStatusAndInternalProcessRequestIdTest() {
    internalProcessServiceImpl
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(STORE_ID, ProcessStatus.PENDING.name(), INTERNAL_ID);
    Mockito.verify(bulkInternalProcessDataRepository)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(STORE_ID, ProcessStatus.PENDING.name(), INTERNAL_ID);
  }

  @Test
  public void getBulkInternalProcessByProcessTypeAndStatusAndCreatedDateTest() {
    Date date = new Date();
    List<String> status = Arrays
        .asList(ProcessStatus.CANCELLED.name(), ProcessStatus.COMPLETED.name(), ProcessStatus.PARTIAL_COMPLETED.name(),
            ProcessStatus.FAILED.name());
    Pageable pageable = PageRequest.of(0, 1);

    Mockito.when(bulkInternalProcessRepository.findByStoreIdAndProcessTypeAndCreatedDateLessThanAndStatusIn(STORE_ID,
        BulkInternalProcessType.STORE_COPY.name(), date, status, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess)));

    internalProcessServiceImpl
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(STORE_ID, BulkInternalProcessType.STORE_COPY.name(),
            date, status, pageable);

    Mockito.verify(bulkInternalProcessRepository).findByStoreIdAndProcessTypeAndCreatedDateLessThanAndStatusIn(STORE_ID,
        BulkInternalProcessType.STORE_COPY.name(), date, status, pageable);
  }

  @Test
  public void deleteBulkInternalProcessTest() {
    Mockito.doNothing().when(bulkInternalProcessRepository).deleteAll(Arrays.asList(bulkInternalProcess));

    internalProcessServiceImpl.deleteBulkInternalProcess(Arrays.asList(bulkInternalProcess));

    Mockito.verify(bulkInternalProcessRepository).deleteAll(Arrays.asList(bulkInternalProcess));
  }

  @Test
  public void deleteBulkInternalProcessDataByInternalRequestIdTest() {
    Mockito.doNothing().when(bulkInternalProcessDataRepository)
        .deleteByInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceImpl.deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    Mockito.verify(bulkInternalProcessDataRepository).deleteByInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  @Test
  public void abortPendingBulkInternalProcessTest() {
    Date abortDate = new Date();
    internalProcessServiceImpl
        .abortPendingBulkInternalProcess(STORE_ID, abortDate, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(bulkInternalProcessRepository)
        .updateStatusInProgressBulkInternalProcessToAborted(abortDate, BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void failPendingBulkInternalProcessDataTest() {
    Date abortDate = new Date();
    internalProcessServiceImpl
        .failPendingBulkInternalProcessData(STORE_ID, abortDate, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(bulkInternalProcessDataRepository)
        .updateStatusInProgressBulkInternalProcessToFailed(abortDate, BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void bulkInternalProcessCancelRequestTest() {
    internalProcessServiceImpl.bulkInternalProcessCancelRequest(STORE_ID, USER_NAME, INTERNAL_PROCESS_REQUEST_CODE);
    Mockito.verify(bulkInternalProcessRepository)
        .bulkInternalProcessCancelRequest(STORE_ID, USER_NAME, INTERNAL_PROCESS_REQUEST_CODE,
            CANCELLED_BY_USER + USER_NAME);
  }

  @Test
  public void getBulkInternalProcessDataByRequestIdsTest() {
    Mockito.when(bulkInternalProcessDataRepository
        .findByStoreIdAndMarkForDeleteFalseAndInternalProcessRequestIdIn(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), 1)).thenReturn(new ArrayList<>());
    internalProcessServiceImpl
        .getBulkInternalProcessDataByRequestIds(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), 1);
    Mockito.verify(bulkInternalProcessDataRepository)
        .findByStoreIdAndMarkForDeleteFalseAndInternalProcessRequestIdIn(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), 1);
  }

  @Test
  public void getBulkInternalProcessDataByRequestIdsAndStatusTest() throws Exception {
    internalProcessServiceImpl.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), 2);
    Mockito.verify(bulkInternalProcessDataRepository)
        .findByStoreIdAndInternalProcessRequestIdInAndStatusAndMarkForDeleteFalse(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
            PageRequest.of(0, 2, Sort.by(Sort.Direction.ASC, Constant.CREATED_DATE)));
  }

  @Test
  public void saveBulkInternalProcessDataTest() {
    Mockito.when(bulkInternalProcessDataRepository.save(new BulkInternalProcessData()))
        .thenReturn(new BulkInternalProcessData());
    internalProcessServiceImpl.saveBulkInternalProcessData(new BulkInternalProcessData());
    Mockito.verify(bulkInternalProcessDataRepository).save(new BulkInternalProcessData());
  }

  @Test
  public void findByInternalProcessRequestCode() {
    Mockito.when(bulkInternalProcessRepository
        .findByStoreIdAndInternalProcessRequestCode(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE))
        .thenReturn(new BulkInternalProcess());
    internalProcessServiceImpl.findByInternalProcessRequestCode(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE);
    Mockito.verify(bulkInternalProcessRepository)
        .findByStoreIdAndInternalProcessRequestCode(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE);
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeAndRowNumberIn() throws Exception {
    Mockito.when(bulkInternalProcessDataRepository
        .findByStoreIdAndInternalProcessRequestCodeAndParentCodeIn(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE,
            Arrays.asList(PRODUCT_CODE))).thenReturn(new ArrayList<>());
    internalProcessServiceImpl.findByStoreIdAndBulkProcessCodeAndRowNumberIn(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE,
        Arrays.asList(PRODUCT_CODE));
    Mockito.verify(bulkInternalProcessDataRepository)
        .findByStoreIdAndInternalProcessRequestCodeAndParentCodeIn(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE,
            Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void checkPendingFilesTrueTest() {
    Mockito.when(bulkInternalProcessRepository
                    .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
            .thenReturn(Arrays.asList(bulkInternalProcess));
    InternalProcessPendingFilesResponse result =
            internalProcessServiceImpl.checkPendingFiles(STORE_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(bulkInternalProcessRepository)
            .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(STORE_ID, USER_NAME,
              BulkInternalProcessType.STORE_COPY.name(),
              Arrays.asList(ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name(),
                ProcessStatus.PUBLISHED.name(),ProcessStatus.PICKED.name()));
    Assertions.assertEquals(result.getIsFilesPending(), true);
  }

  @Test
  public void checkPendingFilesFalseTest() {
    Mockito.when(bulkInternalProcessRepository
                    .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
            .thenReturn(new ArrayList<>());
    InternalProcessPendingFilesResponse result =
            internalProcessServiceImpl.checkPendingFiles(STORE_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(bulkInternalProcessRepository)
            .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(STORE_ID, USER_NAME,
              BulkInternalProcessType.STORE_COPY.name(),
              Arrays.asList(ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name(),
                ProcessStatus.PUBLISHED.name(), ProcessStatus.PICKED.name()));
    Assertions.assertEquals(result.getIsFilesPending(), false);
  }

  @Test
  public void bulkInternalProcessDataByIdAndStatusTest() {
    Mockito.when(bulkInternalProcessDataRepository.findByStoreIdAndIdAndStatus(STORE_ID,
        INTERNAL_ID, STATUS)).thenReturn(BulkInternalProcessData.builder().build());
    internalProcessServiceImpl.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_ID, STATUS);
    Mockito.verify(bulkInternalProcessDataRepository).findByStoreIdAndIdAndStatus(STORE_ID, INTERNAL_ID, STATUS);
  }

  @Test
  public void checkPendingFbbL5ProcessTest() {
    Mockito.when(bulkInternalProcessRepository
      .findFirstByStoreIdAndSellerCodeAndNotesAndStatusIn(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(bulkInternalProcess);
    internalProcessServiceImpl.checkPendingFbbL5Process(STORE_ID, SELLER_CODE, NOTES);
    Mockito.verify(bulkInternalProcessRepository)
      .findFirstByStoreIdAndSellerCodeAndNotesAndStatusIn(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatusTest() {
    internalProcessServiceImpl
      .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(STORE_ID,
        INTERNAL_PROCESS, STATUS);
    Mockito.verify(bulkInternalProcessDataRepository)
      .findByStoreIdAndInternalProcessRequestIdAndStatus(STORE_ID, INTERNAL_PROCESS, STATUS);
  }

  @Test
  public void getAllBulkInternalProcessByStatusOrderByDateAscTest() {
    internalProcessServiceImpl.getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, STATUS,
        PageRequest.of(PAGE, BATCH_SIZE), INTERNAL_PROCESS);
    Mockito.verify(bulkInternalProcessRepository)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalseOrderByCreatedDateAsc(STORE_ID, STATUS,
            INTERNAL_PROCESS, PageRequest.of(PAGE, BATCH_SIZE));
  }

  @Test
  public void findFirstByStoreIdAndProcessTypeAndStatusInTest() {
    Mockito.when(bulkInternalProcessRepository.findFirstByStoreIdAndProcessTypeAndStatusIn(Mockito.anyString(),Mockito.anyString(),Mockito.anyList())).thenReturn(bulkInternalProcess);
    internalProcessServiceImpl.findFirstByStoreIdAndProcessTypeAndStatusIn(STORE_ID,
      BulkProcessType.VENDOR_BULK_ASSIGN.getValue(),
     Arrays.asList(ProcessStatus.PENDING.name()));
    Mockito.verify(bulkInternalProcessRepository)
      .findFirstByStoreIdAndProcessTypeAndStatusIn(STORE_ID, BulkProcessType.VENDOR_BULK_ASSIGN.getValue(),
        Collections.singletonList(ProcessStatus.PENDING.name()));
  }

  @Test
  public void getDistinctCategoryCodeForRestrictedKeywordBulkUpdateTest() {
    Mockito.when(bulkInternalProcessDataRepository.findParentCodeByStoreIdAndInternalProcessRequestIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID)).thenReturn(ImmutableSet.of(PARENT_CODE));
    internalProcessServiceImpl.getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(bulkInternalProcessDataRepository)
        .findParentCodeByStoreIdAndInternalProcessRequestIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
  }

  @Test
  public void bulkInterUpdatedStatusForRestrictedKeywordBulkUpdateTest() {
    Mockito.doNothing().when(bulkInternalProcessDataRepository)
        .updateStatusByParentCodeAndStoreIdAndInternalProcessRequestIdAndStatus(STATUS, USER_NAME, STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ImmutableSet.of(PARENT_CODE));
    internalProcessServiceImpl.bulkInterUpdatedStatusForRestrictedKeywordBulkUpdate(STATUS, USER_NAME, STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ImmutableSet.of(PARENT_CODE));
    Mockito.verify(bulkInternalProcessDataRepository).updateStatusByParentCodeAndStoreIdAndInternalProcessRequestIdAndStatus(STATUS, USER_NAME, STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ImmutableSet.of(PARENT_CODE));
    ;
  }

  @Test
  public void checkPendingFilesForVendorApprovalTrueTest() {
    Mockito.when(bulkInternalProcessRepository.findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    InternalProcessPendingFilesResponse result =
      internalProcessServiceImpl.checkPendingFiles(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(bulkInternalProcessRepository)
      .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_APPROVAL.name(),
        Arrays.asList(ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name(),
          ProcessStatus.PUBLISHED.name(), ProcessStatus.PICKED.name()));
    Assertions.assertEquals(result.getIsFilesPending(), true);
  }

  @Test
  public void checkPendingFilesForVendorRejectionTrueTest() {
    Mockito.when(bulkInternalProcessRepository.findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    InternalProcessPendingFilesResponse result =
      internalProcessServiceImpl.checkPendingFiles(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_REJECTION.name());
    Mockito.verify(bulkInternalProcessRepository)
      .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_REJECTION.name(),
        Arrays.asList(ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name(),
          ProcessStatus.PUBLISHED.name(), ProcessStatus.PICKED.name()));
    Assertions.assertEquals(result.getIsFilesPending(), true);
  }

  @Test
  public void checkPendingFilesTrueBulkApprovalTest() {
    Mockito.when(bulkInternalProcessRepository
                    .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
            .thenReturn(Collections.singletonList(bulkInternalProcess));
    InternalProcessPendingFilesResponse result =
            internalProcessServiceImpl.checkPendingFiles(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(bulkInternalProcessRepository)
            .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(STORE_ID, USER_NAME,
                    BulkInternalProcessType.BULK_APPROVAL.name(),
                    Arrays.asList(ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name(),
                            ProcessStatus.PUBLISHED.name(), ProcessStatus.PICKED.name()));
    Assertions.assertEquals(result.getIsFilesPending(), true);
  }

  @Test
  public void checkPendingFilesFalseBulkApprovalTest() {
    ReflectionTestUtils.setField(internalProcessServiceImpl, "concurrentVendorActionFileCount", 3);
    List<BulkInternalProcess> bulkInternalProcesses = Arrays.asList(bulkInternalProcess,bulkInternalProcess);
    Mockito.when(bulkInternalProcessRepository
                    .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
            .thenReturn(bulkInternalProcesses);
    InternalProcessPendingFilesResponse result =
            internalProcessServiceImpl.checkPendingFiles(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(bulkInternalProcessRepository)
            .findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(STORE_ID, USER_NAME,
                    BulkInternalProcessType.BULK_APPROVAL.name(),
                    Arrays.asList(ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name(),
                            ProcessStatus.PUBLISHED.name(), ProcessStatus.PICKED.name()));
    Assertions.assertEquals(result.getIsFilesPending(), false);
  }

  @Test
  public void updateRemoveBulkProductTypeTaggingTestWithNoFailures(){
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestUpdate =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("Yes").itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).id(INTERNAL_ID).productTypeTagging("KVL").build();
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestRemove =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("No").itemSku(ITEM_SKU.concat("-1"))
        .pickupPointCode(PICKUP_POINT_CODE.concat("-1")).id(INTERNAL_ID).productTypeTagging("KVL").build();
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      List.of(priceProductTypeTaggingRequestUpdate, priceProductTypeTaggingRequestRemove);
    UpdateRemoveProductTaggingRequest updateProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL")
        .itemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE)).build();
    UpdateRemoveProductTaggingRequest removeProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL").itemPickUpPointId(
          ITEM_SKU.concat("-1").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE).concat("-1"))
        .build();
    Mockito.when(priceAnalyticsOutboundService.removeTagging("user",
      Collections.singletonList(removeProductTaggingRequest))).thenReturn(Collections.emptyList());
    Mockito.when(priceAnalyticsOutboundService.updateTagging("user",
      Collections.singletonList(updateProductTaggingRequest))).thenReturn(Collections.emptyList());
    Map<String, String> rowXErrorMessage =
      internalProcessServiceImpl.updateRemoveBulkProductTypeTagging(
        bulkPriceProductTypeTaggingRequests, "user");
    Mockito.verify(priceAnalyticsOutboundService).updateTagging("user",
      Collections.singletonList(removeProductTaggingRequest));
    Mockito.verify(priceAnalyticsOutboundService).removeTagging("user",
      Collections.singletonList(updateProductTaggingRequest));
  }

  @Test
  public void updateRemoveBulkProductTypeTaggingTestWithFailures(){
    FailedReasonResponse failedReasonResponseForUpdate = new FailedReasonResponse();
    failedReasonResponseForUpdate.setFailedReason("Failure reason");
    failedReasonResponseForUpdate.setItemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE));
    FailedReasonResponse failedReasonResponseForRemoval = new FailedReasonResponse();
    failedReasonResponseForRemoval.setFailedReason("Failure reason removal");
    String concat =
      ITEM_SKU.concat("-1").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE).concat("-1");
    failedReasonResponseForRemoval.setItemPickUpPointId(concat);
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestUpdate =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("Yes").itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).id(INTERNAL_ID).productTypeTagging("KVL").build();
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestRemove =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("No").itemSku(ITEM_SKU.concat("-1"))
        .pickupPointCode(PICKUP_POINT_CODE.concat("-1")).id(INTERNAL_ID).productTypeTagging("KVL").build();
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      List.of(priceProductTypeTaggingRequestUpdate, priceProductTypeTaggingRequestRemove);
    UpdateRemoveProductTaggingRequest updateProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL")
        .itemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE)).build();
    UpdateRemoveProductTaggingRequest removeProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL").itemPickUpPointId(concat)
        .build();
    Mockito.when(priceAnalyticsOutboundService.removeTagging("user",
      Collections.singletonList(removeProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForRemoval));
    Mockito.when(priceAnalyticsOutboundService.updateTagging("user",
      Collections.singletonList(updateProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForUpdate));
    Map<String, String> rowXErrorMessage =
      internalProcessServiceImpl.updateRemoveBulkProductTypeTagging(
        bulkPriceProductTypeTaggingRequests, "user");
    Mockito.verify(priceAnalyticsOutboundService).updateTagging("user",
      Collections.singletonList(removeProductTaggingRequest));
    Mockito.verify(priceAnalyticsOutboundService).removeTagging("user",
      Collections.singletonList(updateProductTaggingRequest));
  }

  @Test
  public void updateRemoveBulkProductTypeTaggingTestWithNoUpdateType(){
    FailedReasonResponse failedReasonResponseForUpdate = new FailedReasonResponse();
    failedReasonResponseForUpdate.setFailedReason("Failure reason");
    failedReasonResponseForUpdate.setItemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE));
    FailedReasonResponse failedReasonResponseForRemoval = new FailedReasonResponse();
    failedReasonResponseForRemoval.setFailedReason("Failure reason removal");
    String concat =
      ITEM_SKU.concat("-1").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE).concat("-1");
    failedReasonResponseForRemoval.setItemPickUpPointId(concat);
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestUpdate =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("fake").itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).id(INTERNAL_ID).productTypeTagging("KVL").build();
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestRemove =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("fake").itemSku(ITEM_SKU.concat("-1"))
        .pickupPointCode(PICKUP_POINT_CODE.concat("-1")).id(INTERNAL_ID).productTypeTagging("KVL").build();
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      List.of(priceProductTypeTaggingRequestUpdate, priceProductTypeTaggingRequestRemove);
    UpdateRemoveProductTaggingRequest updateProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL")
        .itemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE)).build();
    UpdateRemoveProductTaggingRequest removeProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL").itemPickUpPointId(concat)
        .build();
    Mockito.when(priceAnalyticsOutboundService.removeTagging("user",
      Collections.singletonList(removeProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForRemoval));
    Mockito.when(priceAnalyticsOutboundService.updateTagging("user",
      Collections.singletonList(updateProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForUpdate));
    Map<String, String> rowXErrorMessage =
      internalProcessServiceImpl.updateRemoveBulkProductTypeTagging(
        bulkPriceProductTypeTaggingRequests, "user");
    Mockito.verify(priceAnalyticsOutboundService).updateTagging("user",
      List.of(updateProductTaggingRequest, removeProductTaggingRequest));
  }

  @Test
  public void updateRemoveBulkProductTypeTaggingTestWithNoRemoveType(){
    FailedReasonResponse failedReasonResponseForUpdate = new FailedReasonResponse();
    failedReasonResponseForUpdate.setFailedReason("Failure reason");
    failedReasonResponseForUpdate.setItemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE));
    FailedReasonResponse failedReasonResponseForRemoval = new FailedReasonResponse();
    failedReasonResponseForRemoval.setFailedReason("Failure reason removal");
    String concat =
      ITEM_SKU.concat("-1").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE).concat("-1");
    failedReasonResponseForRemoval.setItemPickUpPointId(concat);
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestUpdate =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("yes").itemSku(ITEM_SKU.concat("-1"))
        .pickupPointCode(PICKUP_POINT_CODE).id(INTERNAL_ID).productTypeTagging("KVL").build();
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestRemove =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("yes").itemSku(ITEM_SKU.concat("-1"))
        .pickupPointCode(PICKUP_POINT_CODE.concat("-1")).id(INTERNAL_ID).productTypeTagging("KVL").build();
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      List.of(priceProductTypeTaggingRequestUpdate, priceProductTypeTaggingRequestRemove);
    UpdateRemoveProductTaggingRequest updateProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL")
        .itemPickUpPointId(ITEM_SKU.concat("-2").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE)).build();
    UpdateRemoveProductTaggingRequest removeProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL").itemPickUpPointId(concat)
        .build();
    failedReasonResponseForUpdate.setItemPickUpPointId(ITEM_SKU);
    Mockito.when(priceAnalyticsOutboundService.removeTagging("user",
      Collections.singletonList(removeProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForRemoval));
    Mockito.when(priceAnalyticsOutboundService.updateTagging("user",
      Collections.singletonList(updateProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForUpdate));
    Map<String, String> rowXErrorMessage =
      internalProcessServiceImpl.updateRemoveBulkProductTypeTagging(
        bulkPriceProductTypeTaggingRequests, "user");
    Mockito.verify(priceAnalyticsOutboundService).removeTagging(Mockito.anyString(),
      Mockito.anyList());
  }

  @Test
  public void updateRemoveBulkProductTypeTaggingTestWithMismatchId(){
    FailedReasonResponse failedReasonResponseForUpdate = new FailedReasonResponse();
    failedReasonResponseForUpdate.setFailedReason("Failure reason");
    failedReasonResponseForUpdate.setItemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE));
    FailedReasonResponse failedReasonResponseForRemoval = new FailedReasonResponse();
    failedReasonResponseForRemoval.setFailedReason("Failure reason removal");
    String concat =
      ITEM_SKU.concat("-1").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE).concat("-1");
    failedReasonResponseForRemoval.setItemPickUpPointId(concat);
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestUpdate =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("Yes").itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).id(INTERNAL_ID).productTypeTagging("KVL").build();
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestRemove =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("No").itemSku(ITEM_SKU.concat("-1"))
        .pickupPointCode(PICKUP_POINT_CODE.concat("-1")).id(INTERNAL_ID).productTypeTagging("KVL").build();
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      List.of(priceProductTypeTaggingRequestUpdate, priceProductTypeTaggingRequestRemove);
    UpdateRemoveProductTaggingRequest updateProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL")
        .itemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE)).build();
    UpdateRemoveProductTaggingRequest removeProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL").itemPickUpPointId(concat.concat("-1"))
        .build();
    failedReasonResponseForRemoval.setItemPickUpPointId(ITEM_SKU);
    failedReasonResponseForRemoval.setFailedReason("Reason");
    Mockito.when(priceAnalyticsOutboundService.removeTagging("user",
      Collections.singletonList(removeProductTaggingRequest))).thenReturn(List.of(failedReasonResponseForRemoval, failedReasonResponseForUpdate));
    Mockito.when(priceAnalyticsOutboundService.updateTagging("user",
      Collections.singletonList(updateProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForUpdate));
    Map<String, String> rowXErrorMessage =
      internalProcessServiceImpl.updateRemoveBulkProductTypeTagging(
        bulkPriceProductTypeTaggingRequests, "user");
    Mockito.verify(priceAnalyticsOutboundService).updateTagging(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(priceAnalyticsOutboundService).removeTagging(Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void updateRemoveBulkProductTypeTaggingTestWithEmptyResponse(){
    FailedReasonResponse failedReasonResponseForUpdate = new FailedReasonResponse();
    failedReasonResponseForUpdate.setFailedReason("Failure reason");
    failedReasonResponseForUpdate.setItemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE));
    FailedReasonResponse failedReasonResponseForRemoval = new FailedReasonResponse();
    failedReasonResponseForRemoval.setFailedReason("Failure reason removal");
    String concat =
      ITEM_SKU.concat("-1").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE).concat("-1");
    failedReasonResponseForRemoval.setItemPickUpPointId(concat);
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestUpdate =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("Yes").itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).id(INTERNAL_ID).productTypeTagging("KVL").build();
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestRemove =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("No").itemSku(ITEM_SKU.concat("-1"))
        .pickupPointCode(PICKUP_POINT_CODE.concat("-1")).id(INTERNAL_ID).productTypeTagging("KVL").build();
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      List.of(priceProductTypeTaggingRequestUpdate, priceProductTypeTaggingRequestRemove);
    UpdateRemoveProductTaggingRequest updateProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL")
        .itemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE)).build();
    UpdateRemoveProductTaggingRequest removeProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL").itemPickUpPointId(concat.concat("-1"))
        .build();
    failedReasonResponseForRemoval.setItemPickUpPointId(ITEM_SKU);
    failedReasonResponseForRemoval.setFailedReason("Reason");
    Mockito.when(priceAnalyticsOutboundService.removeTagging("user",
      Collections.singletonList(removeProductTaggingRequest))).thenReturn(Collections.emptyList());
    Mockito.when(priceAnalyticsOutboundService.updateTagging("user",
      Collections.singletonList(updateProductTaggingRequest))).thenReturn(Collections.singletonList(failedReasonResponseForUpdate));
    Map<String, String> rowXErrorMessage =
      internalProcessServiceImpl.updateRemoveBulkProductTypeTagging(
        bulkPriceProductTypeTaggingRequests, "user");
    Mockito.verify(priceAnalyticsOutboundService).updateTagging(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(priceAnalyticsOutboundService).removeTagging(Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void updateRemoveBulkProductTypeTaggingTestWithFailures2(){
    FailedReasonResponse failedReasonResponseForUpdate = new FailedReasonResponse();
    failedReasonResponseForUpdate.setFailedReason("Failure reason");
    failedReasonResponseForUpdate.setItemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE));
    FailedReasonResponse failedReasonResponseForRemoval = new FailedReasonResponse();
    failedReasonResponseForRemoval.setFailedReason("Failure reason removal");
    String concat = ITEM_SKU.concat("-1").concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE).concat("-1");
    failedReasonResponseForRemoval.setItemPickUpPointId(concat);
    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestUpdate =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("Yes").itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).id(INTERNAL_ID).productTypeTagging("KVL").build();

    BulkPriceProductTypeTaggingRequest priceProductTypeTaggingRequestRemove =
      BulkPriceProductTypeTaggingRequest.builder().deleteProductTypeTagging("No")
        .itemSku(ITEM_SKU.concat("-1")).pickupPointCode(PICKUP_POINT_CODE.concat("-1"))
        .id(INTERNAL_ID).productTypeTagging("KVL").build();

    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      List.of(priceProductTypeTaggingRequestUpdate, priceProductTypeTaggingRequestRemove);

    UpdateRemoveProductTaggingRequest updateProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL")
        .itemPickUpPointId(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE)).build();

    UpdateRemoveProductTaggingRequest removeProductTaggingRequest =
      UpdateRemoveProductTaggingRequest.builder().productType("KVL").itemPickUpPointId(concat)
        .build();
    Mockito.when(priceAnalyticsOutboundService.removeTagging(Mockito.anyString(),
        Mockito.anyList()))
      .thenReturn(Collections.singletonList(failedReasonResponseForRemoval));
    Mockito.when(priceAnalyticsOutboundService.updateTagging(Mockito.anyString(),
        Mockito.anyList()))
      .thenReturn(Collections.singletonList(failedReasonResponseForUpdate));
    Map<String, String> rowXErrorMessage = internalProcessServiceImpl.updateRemoveBulkProductTypeTagging(
      bulkPriceProductTypeTaggingRequests, "user");
    Mockito.verify(priceAnalyticsOutboundService).updateTagging("user",
      Collections.singletonList(removeProductTaggingRequest));
    Mockito.verify(priceAnalyticsOutboundService).removeTagging("user",
      Collections.singletonList(updateProductTaggingRequest));
  }

  @Test
  public void countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalseTest() {
    internalProcessServiceImpl.countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE, Collections.singletonList(ProcessStatus.PENDING.name()), USER_NAME);
    Mockito.verify(bulkInternalProcessRepository)
        .countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE,
            Collections.singletonList(ProcessStatus.PENDING.name()), USER_NAME);
  }

  @Test
  public void countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalseEmptyTypeTest() {
    internalProcessServiceImpl.countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(STORE_ID,
        StringUtils.EMPTY, Collections.singletonList(ProcessStatus.PENDING.name()), USER_NAME);
    Mockito.verify(bulkInternalProcessRepository).countByStoreIdAndStatusInAndCreatedByAndMarkForDeleteFalse(STORE_ID,
        Collections.singletonList(ProcessStatus.PENDING.name()), USER_NAME);
  }
}