package com.gdn.mta.bulk.service;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.BulkProcessEstimationResponseDTO;
import com.gdn.mta.bulk.entity.BulkProcessDataEstimation;
import com.gdn.mta.bulk.repository.BulkProcessDataEstimationRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

public class BulkProcessEstimationsServiceBeanTest {

    @InjectMocks
    private BulkProcessEstimationServiceBean bulkProcessEstimationService;

    @Mock
    private BulkProcessDataEstimationRepository dataEstimationRepository;

    @Mock
    private BulkProcessRepository processRepository;

    @Mock
    private ObjectMapper objectMapper;

    @BeforeEach
    public void setUp() {
      MockitoAnnotations.initMocks(this);
      bulkProcessEstimationService.bulkDataEstimationRepository = dataEstimationRepository;
      bulkProcessEstimationService.bulkProcessRepository = processRepository;
      bulkProcessEstimationService.objectMapper = objectMapper;
      ReflectionTestUtils.setField(bulkProcessEstimationService,
        "additionalFetchDaysMultiplierForEstimations","2");
    }

    @AfterEach
    public void after(){
      verifyNoMoreInteractions(processRepository,dataEstimationRepository);
    }

    @Test
    public void testUpdateBulkDataEstimationByProcessTypesForRecordLevelFetch() {
      ReflectionTestUtils.setField(bulkProcessEstimationService,
        "maxFetchDaysForProcessTimeEstimation","7");
      ReflectionTestUtils.setField(bulkProcessEstimationService,
        "deltaTimesForEstimateEvaluation","1");
      ReflectionTestUtils.setField(bulkProcessEstimationService,
        "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
      when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean()))
        .thenReturn(null);
      when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList()))
        .thenReturn(Collections.singletonList(new Object[] {1, 10L, 100L, 5000L, "recordLevelFetch"}));
      assertDoesNotThrow(() ->
        bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId", "username"));
      verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(any()
        , anyBoolean());
      verify(dataEstimationRepository, times(1)).save(any());
      verify(processRepository, times(3)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());
    }

  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForRecordLevelFetchWithZeroFetch() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation","7");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "deltaTimesForEstimateEvaluation","1");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean()))
      .thenReturn(null);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList()))
      .thenReturn(Collections.singletonList(new Object[] {1, 0L, 0L, 0L, "recordLevelFetch"}));
    assertDoesNotThrow(() ->
      bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId", "username"));
    verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(any()
      , anyBoolean());
    verify(dataEstimationRepository, times(1)).save(any());
    verify(processRepository, times(3)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());
  }

  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForRecordLevelFetchWithUpdate() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation", "7");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "deltaTimesForEstimateEvaluation",
      "1");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "supportedProcessTypeForEstimations",
      "ProductCreationUploadPriority1");
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(),
      anyBoolean())).thenReturn(bulkProcessDataEstimation);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList())).thenReturn(
      Collections.singletonList(new Object[] {1, 10L, 100L, 5000L, "recordLevelFetch"}));
    assertDoesNotThrow(
      () -> bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId",
        "username"));
    verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(any(),
      anyBoolean());
    verify(dataEstimationRepository, times(1)).save(any());
    verify(processRepository, times(3)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());
  }

  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForProcessLevelFetchWithUpdate() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation", "7");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "deltaTimesForEstimateEvaluation",
      "1");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "supportedProcessTypeForEstimations",
      "ProductCreationUploadPriority1");
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(true).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(),
      anyBoolean())).thenReturn(bulkProcessDataEstimation);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList())).thenReturn(
      Collections.singletonList(new Object[] {1, 10L, 100L, 5000L, "processLevelFetch"}));
    assertDoesNotThrow(
      () -> bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId",
        "username"));
    verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(any(),
      anyBoolean());
    verify(dataEstimationRepository, times(1)).save(any());
    verify(processRepository, times(3)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());
  }

  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForProcessLevelFetch() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation","7");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "deltaTimesForEstimateEvaluation","1");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean()))
      .thenReturn(null);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList()))
      .thenReturn(Collections.singletonList(new Object[] {1, 10L, 100L, 5000L,
        "processLevelFetch"}));
    assertDoesNotThrow(() ->
      bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId", "username"));
    verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(any()
      , anyBoolean());
    verify(dataEstimationRepository, times(1)).save(any());
    verify(processRepository, times(3)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());
  }


  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForMissingProcessLevelFetchProcessLevelFetch() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation","7");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "additionalFetchDaysMultiplierForEstimations","2");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "deltaTimesForEstimateEvaluation","1");
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE,
      -(Integer.parseInt("7") * Integer.parseInt(
        "2")));
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1,InStore");
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean()))
      .thenReturn(null);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList()))
      .thenReturn(Collections.singletonList(new Object[] {1, 10L, 100L, 5000L,
        "processLevelFetch"}));
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(eq(calendar.getTime()),
      eq(
      "InStore"),
      eq(List.of("ProductCreationUploadPriority1,InStore"))))
      .thenReturn(Arrays.asList(new Object[] {1, 10L, 100L, 5000L,
        "processLevelFetch"},new Object[]{1, 10L, 100L, 5000L,
        "InStore"}));
    assertDoesNotThrow(() ->
      bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId", "username"));
    verify(dataEstimationRepository, times(2)).findFirstByProcessTypeAndProcessLevelFetch(any()
      , anyBoolean());
    verify(dataEstimationRepository, times(2)).save(any());
    verify(processRepository, times(6)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());
  }


  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForProcessLevelFetchWithZeroRecordFetched() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation","7");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "deltaTimesForEstimateEvaluation","1");
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean()))
      .thenReturn(null);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList()))
      .thenReturn(Collections.singletonList(new Object[] {1, 0L, 0L, 0L,
        "processLevelFetch"}));
    assertDoesNotThrow(() ->
      bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId", "username"));
    verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(any()
      , anyBoolean());
    verify(dataEstimationRepository, times(1)).save(any());
    verify(processRepository, times(3)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());
  }

  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForProcessLevelFetchWithNullRecordFetched()
    throws JsonProcessingException {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation", "7");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "deltaTimesForEstimateEvaluation",
      "1");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "supportedProcessTypeForEstimations",
      "ProductCreationUploadPriority1");
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean())).thenReturn(null);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(),
      anyList())).thenReturn(
      Collections.singletonList(new Object[] {1, null, null, null, "processLevelFetch"}));
   bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId",
      "username");
    verify(processRepository, times(4)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());

  }

  @Test
  public void testUpdateBulkDataEstimationByProcessTypesForProcessLevelFetchWithLessRows()
    throws JsonProcessingException {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "maxFetchDaysForProcessTimeEstimation", "7");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "deltaTimesForEstimateEvaluation",
      "1");
    ReflectionTestUtils.setField(bulkProcessEstimationService, "supportedProcessTypeForEstimations",
      "ProductCreationUploadPriority1");
    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean())).thenReturn(null);
    when(processRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(),
      anyList())).thenReturn(
      Collections.singletonList(new Object[] {1, 200L, "processLevelFetch"}));
    bulkProcessEstimationService.updateBulkDataEstimationByProcessTypes("storeId",
      "username");
    verify(processRepository, times(4)).getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(any(), any(), anyList());

  }

  @Test
  public void testPerformUpdatesByProcessTypeAndFetchType_ProcessLevelFetch() throws Exception {
    List<BulkProcessEstimationResponseDTO> estimations = new ArrayList<>();
    estimations.add(new BulkProcessEstimationResponseDTO(1, 10L, 0L, 5000L, "processType", "processLevelFetch"));
    when(objectMapper.writeValueAsString(anyMap())).thenReturn("{\"1\": 500.0}");
    bulkProcessEstimationService.performUpdatesByProcessTypeAndFetchType("processType", "processLevelFetch", estimations);
    verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(eq("processType"), eq(true));
    verify(dataEstimationRepository, times(1)).save(any());
  }

  @Test
  public void testUpdateOrCreateByProcessTypeForBulkProcessEstimations_ProcessLevelFetch() {
    BulkProcessDataEstimation savedEstimation = new BulkProcessDataEstimation();
    savedEstimation.setProcessType("processType");
    savedEstimation.setProcessLevelFetch(true);

    when(dataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(any(), anyBoolean()))
      .thenReturn(savedEstimation);

    bulkProcessEstimationService.updateOrCreateByProcessTypeForBulkProcessEstimations("processType", "{}", new Date(), true);
    verify(dataEstimationRepository, times(1)).findFirstByProcessTypeAndProcessLevelFetch(eq("processType"), eq(true));
    verify(dataEstimationRepository, times(1)).save(any());
    }


@Test
    public void testGetSupportedProcessTypesForEstimateEvaluation() {
      bulkProcessEstimationService.supportedProcessTypeForEstimations = "type1,type2,type3";
      List<String> expectedTypes = Arrays.asList("type1", "type2", "type3");
      List<String> actualTypes = bulkProcessEstimationService.getSupportedProcessTypesForEstimateEvaluation();
      Assertions.assertEquals(expectedTypes, actualTypes);
    }

  @Test
  public void testFetchAllEstimationResponsesByProcessTypes_ValidProcessTypes() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
    List<String> supportedProcessTypes = Arrays.asList("ProductCreationUploadPriority1");
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    when(dataEstimationRepository.findByProcessTypeInAndMarkForDeleteFalse(anyList())).thenReturn(
      Collections.singletonList(bulkProcessDataEstimation));
    List<BulkProcessDataEstimation> result =
      bulkProcessEstimationService.fetchAllEstimationResponsesByProcessTypes(supportedProcessTypes);
    verify(dataEstimationRepository).findByProcessTypeInAndMarkForDeleteFalse(anyList());
  }

  @Test
  public void testFetchAllEstimationResponsesByProcessTypes_NoSupportedProcessTypes() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
    List<String> unsupportedProcessTypes = Arrays.asList("SomeOtherProcessType");

    List<BulkProcessDataEstimation> result =
      bulkProcessEstimationService.fetchAllEstimationResponsesByProcessTypes(unsupportedProcessTypes);

    Assertions.assertTrue(result.isEmpty());
  }

  @Test
  public void testFetchAllEstimationResponsesByProcessTypes_NoEstimationsFound() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
    List<String> supportedProcessTypes = Arrays.asList("ProductCreationUploadPriority1");

    when(dataEstimationRepository.findByProcessTypeInAndMarkForDeleteFalse(anyList()))
      .thenReturn(Collections.emptyList());

    List<BulkProcessDataEstimation> result =
      bulkProcessEstimationService.fetchAllEstimationResponsesByProcessTypes(supportedProcessTypes);

    Assertions.assertTrue(result.isEmpty());
    verify(dataEstimationRepository).findByProcessTypeInAndMarkForDeleteFalse(anyList());
  }

  @Test
  public void testFetchAllEstimationResponsesByProcessTypes_MixedValidAndInvalidProcessTypes() {
    ReflectionTestUtils.setField(bulkProcessEstimationService,
      "supportedProcessTypeForEstimations","ProductCreationUploadPriority1");
    List<String> mixedProcessTypes = Arrays.asList("ProductCreationUploadPriority1", "SomeOtherProcessType");

    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();

    when(dataEstimationRepository.findByProcessTypeInAndMarkForDeleteFalse(Collections.singletonList("ProductCreationUploadPriority1")))
      .thenReturn(Collections.singletonList(bulkProcessDataEstimation));

    List<BulkProcessDataEstimation> result =
      bulkProcessEstimationService.fetchAllEstimationResponsesByProcessTypes(mixedProcessTypes);

    Assertions.assertFalse(result.isEmpty());
    verify(dataEstimationRepository).findByProcessTypeInAndMarkForDeleteFalse(Collections.singletonList("ProductCreationUploadPriority1"));
  }


}

