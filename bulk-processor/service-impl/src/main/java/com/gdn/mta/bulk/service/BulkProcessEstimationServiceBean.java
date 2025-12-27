package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.dto.BulkProcessEstimationResponseDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessDataEstimation;
import com.gdn.mta.bulk.repository.BulkProcessDataEstimationRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@Slf4j
public class BulkProcessEstimationServiceBean implements BulkProcessDataEstimationService{

  @Value("${supported.process.types.estimations}")
  String supportedProcessTypeForEstimations;

  @Value("${delta.time.for.estimate.evaluation}")
  String deltaTimesForEstimateEvaluation;

  @Value("${max.fetch.days.time.estimation}")
  private String maxFetchDaysForProcessTimeEstimation;
  
  @Value("${additional.fetch.days.multiplier.estimation}") 
  private String additionalFetchDaysMultiplierForEstimations;

  private final String RECORD_LEVEL_FETCH = "recordLevelFetch";
  private final String PROCESS_LEVEL_FETCH = "processLevelFetch";

  @Autowired
  BulkProcessDataEstimationRepository bulkDataEstimationRepository;

  @Autowired
  BulkProcessRepository bulkProcessRepository;

  @Autowired
  ObjectMapper objectMapper;

  private static final List<String> statusesForRecordLevelFetch =
    Arrays.asList(BulkProcess.STATUS_FINISHED, BulkProcess.STATUS_PARTIALLY_DONE,
      BulkProcess.STATUS_FAIL);

  private static final List<String> statusesForProcessLevelFetch =
    Stream.of(BulkProcess.STATUS_FINISHED, BulkProcess.STATUS_PARTIALLY_DONE,
      BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_ABORTED,
      BulkProcess.STATUS_READY_TO_PROCESS, BulkProcess.STATUS_FAIL).collect(Collectors.toList());

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateBulkDataEstimationByProcessTypes(String storeId, String username)
    throws JsonProcessingException {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -Integer.parseInt(maxFetchDaysForProcessTimeEstimation));
    Date createdDateLimit = calendar.getTime();
    List<String> typesForEstimateEvaluation = getSupportedProcessTypesForEstimateEvaluation();
    Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>> processTypeXEstimations =
      new HashMap<>();
    Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>>
      updatedProcessTypeXEstimations = new HashMap<>();

    List<BulkProcessEstimationResponseDTO> bulkProcessEstimationResponseDTOS = new ArrayList<>();

    for (String processType : typesForEstimateEvaluation) {
      List<Object[]> updatedStatisticsForRecordLevelFetch = new ArrayList<>();
      List<Object[]> statisticsForRecordLevelFetch =
        bulkProcessRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(
          createdDateLimit, processType, statusesForRecordLevelFetch);

      log.info("Fetched Statistics for Record Level Fetch with Response : {} ",
        statisticsForRecordLevelFetch);

      List<Object[]> statisticsForProcessLevelFetch =
        bulkProcessRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(
          createdDateLimit, processType, statusesForProcessLevelFetch);

      log.info("Fetched Statistics for Process Level Fetch with Response : {} ",
        statisticsForProcessLevelFetch);

      List<Object[]> updatedStatisticsForProcessLevelFetch =
        getUpdatedStatisticsForProcessLevelFetch(statisticsForRecordLevelFetch,
          statisticsForProcessLevelFetch, updatedStatisticsForRecordLevelFetch);

      // Merging statisticsForRecordLevelFetch and statisticsForProcessLevelFetch
      List<Object[]> mergedStatistics = new ArrayList<>(updatedStatisticsForProcessLevelFetch);
      mergedStatistics.addAll(updatedStatisticsForRecordLevelFetch);

      updatedProcessTypeXEstimations =
        updateProcessTypeEstimationsMapForMergedResponses(processTypeXEstimations, processType,
          mergedStatistics);
      //updatedProcessTypeXEstimations is Map with processType as Key and its Value is a Map with
      // FetchType as Key and Estimations as Value

    }
    computeEstimationsForMissingProcessTypes(updatedProcessTypeXEstimations);

    for (Map.Entry<String, Map<String, List<BulkProcessEstimationResponseDTO>>> processTypeEntry : updatedProcessTypeXEstimations.entrySet()) {
      String processType = processTypeEntry.getKey();
      Map<String, List<BulkProcessEstimationResponseDTO>> fetchTypeEstimationsMap =
        processTypeEntry.getValue();

      for (Map.Entry<String, List<BulkProcessEstimationResponseDTO>> fetchTypeEntry : fetchTypeEstimationsMap.entrySet()) {
        String fetchType = fetchTypeEntry.getKey();
        List<BulkProcessEstimationResponseDTO> estimations = fetchTypeEntry.getValue();

        performUpdatesByProcessTypeAndFetchType(processType, fetchType, estimations);
      }
    }

  }

  private Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>> computeEstimationsForMissingProcessTypes(
    Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>> updatedProcessTypeXEstimations) {
    List<String> presentProcessTypes = new ArrayList<>(updatedProcessTypeXEstimations.keySet());
    Map<String, List<String>> fetchTypeToProcessTypesMap =
      buildFetchTypeToProcessTypesMap(updatedProcessTypeXEstimations);

    List<String> processesEvaluatedForProcessLevelFetch =
      fetchTypeToProcessTypesMap.getOrDefault(PROCESS_LEVEL_FETCH, new ArrayList<>());
    List<String> processesEvaluatedForRecordLevelFetch =
      fetchTypeToProcessTypesMap.getOrDefault(RECORD_LEVEL_FETCH, new ArrayList<>());

    List<String> missingProcessTypesForProcessLevelFetch =
      findMissingProcessTypes(processesEvaluatedForProcessLevelFetch);
    List<String> missingProcessTypesForRecordLevelFetch =
      findMissingProcessTypes(processesEvaluatedForRecordLevelFetch);

    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE,
      -(Integer.parseInt(maxFetchDaysForProcessTimeEstimation) * Integer.parseInt(
        additionalFetchDaysMultiplierForEstimations)));

    updateMissingProcessTypeEstimations(updatedProcessTypeXEstimations,
      missingProcessTypesForProcessLevelFetch, calendar, statusesForProcessLevelFetch);
    updateMissingProcessTypeEstimations(updatedProcessTypeXEstimations,
      missingProcessTypesForRecordLevelFetch, calendar, statusesForRecordLevelFetch);

    return updatedProcessTypeXEstimations;
  }

  private Map<String, List<String>> buildFetchTypeToProcessTypesMap(
    Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>> updatedProcessTypeXEstimations) {
    Map<String, List<String>> fetchTypeToProcessTypesMap = new HashMap<>();
    updatedProcessTypeXEstimations.forEach(
      (processType, fetchTypeEstimationsMap) -> fetchTypeEstimationsMap.forEach(
        (fetchType, estimations) -> fetchTypeToProcessTypesMap.computeIfAbsent(fetchType,
          k -> new ArrayList<>()).add(processType)));
    return fetchTypeToProcessTypesMap;
  }

  private List<String> findMissingProcessTypes(List<String> evaluatedProcessTypes) {
    return getSupportedProcessTypesForEstimateEvaluation().stream()
      .filter(Predicate.not(evaluatedProcessTypes::contains)).collect(Collectors.toList());
  }

  private void updateMissingProcessTypeEstimations(
    Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>> updatedProcessTypeXEstimations,
    List<String> missingProcessTypes, Calendar calendar, List<String> statuses) {
    for (String processType : missingProcessTypes) {
      List<Object[]> estimationForMissingRecordLevelFetch =
        bulkProcessRepository.getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(
          calendar.getTime(), processType, statuses);
      updateProcessTypeEstimationsMapForMergedResponses(updatedProcessTypeXEstimations, processType,
        estimationForMissingRecordLevelFetch);
    }
  }

  @Override
  public List<BulkProcessDataEstimation> fetchAllEstimationResponsesByProcessTypes(
    List<String> processTypes) {
    List<String> supportedProcessTypes = getSupportedProcessTypesForEstimateEvaluation();
    List<String> validProcessTypes =
      processTypes.stream().filter(supportedProcessTypes::contains).collect(Collectors.toList());
    if (!validProcessTypes.isEmpty()) {
      return bulkDataEstimationRepository.findByProcessTypeInAndMarkForDeleteFalse(
        validProcessTypes);
    }
    return Collections.emptyList();
  }



  private List<Object[]> getUpdatedStatisticsForProcessLevelFetch(List<Object[]> statisticsForRecordLevelFetch,
    List<Object[]> statisticsForProcessLevelFetch,
    List<Object[]> updatedStatisticsForRecordLevelFetch) {
    for (Object[] statsArray : statisticsForRecordLevelFetch) {
      int recordObjectSize = statsArray.length;
      Object[] updatedStatsArray = Arrays.copyOf(statsArray, recordObjectSize + 1);
      updatedStatsArray[recordObjectSize] = RECORD_LEVEL_FETCH;
      updatedStatisticsForRecordLevelFetch.add(updatedStatsArray);
    }

    List<Object[]> updatedStatisticsForProcessLevelFetch = new ArrayList<>();
    for (Object[] statsArray : statisticsForProcessLevelFetch) {
      int processObjectSize = statsArray.length;
      Object[] updatedStatsArray = Arrays.copyOf(statsArray, processObjectSize + 1);
      updatedStatsArray[processObjectSize] = PROCESS_LEVEL_FETCH;
      updatedStatisticsForProcessLevelFetch.add(updatedStatsArray);
    }
    return updatedStatisticsForProcessLevelFetch;
  }

  void performUpdatesByProcessTypeAndFetchType(String processType, String fetchType,
    List<BulkProcessEstimationResponseDTO> estimations) throws JsonProcessingException {
    if (fetchType.equals((PROCESS_LEVEL_FETCH))) {
      log.info("Updating the Estimations for Process level Fetch at : {} with data : {}",
        new Date(), estimations);
      Map<Integer, Double> estimationsMappingAtProcessLevelForDbSave =
        estimations.stream().filter(estimation -> estimation.getTotalProcesses() != 0L).collect(
          Collectors.toMap(BulkProcessEstimationResponseDTO::getHour,
            estimation -> (double) estimation.getTotalProcessingTime()
              / estimation.getTotalProcesses(), (a, b) -> a));
      String intervalTimeEstimationsAtProcessLevel =
        objectMapper.writeValueAsString(estimationsMappingAtProcessLevelForDbSave);
      updateOrCreateByProcessTypeForBulkProcessEstimations(processType,
        intervalTimeEstimationsAtProcessLevel, new Date(), true);
    }
    if (fetchType.equals(RECORD_LEVEL_FETCH)) {
      log.info("Updating the Estimations for record level Fetch at : {} with data : {}", new Date(),
        estimations);
      Map<Integer, Double> estimationsMappingAtRecordLevelForDbSave =
        estimations.stream().filter(estimation -> estimation.getTotalRecords() != 0L).collect(
          Collectors.toMap(BulkProcessEstimationResponseDTO::getHour,
            estimation -> (double) estimation.getTotalProcessingTime()
              / estimation.getTotalRecords(), (a, b) -> a));
      String intervalTimeEstimationsAtRecordLevel =
        objectMapper.writeValueAsString(estimationsMappingAtRecordLevelForDbSave);
      updateOrCreateByProcessTypeForBulkProcessEstimations(processType,
        intervalTimeEstimationsAtRecordLevel, new Date(), false);
    }
  }

  private static Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>> updateProcessTypeEstimationsMapForMergedResponses(
    Map<String, Map<String, List<BulkProcessEstimationResponseDTO>>> processTypeXEstimations,
    String processType, List<Object[]> mergedStatistics) {
    Map<String, List<BulkProcessEstimationResponseDTO>> fetchTypeEstimations = new HashMap<>();

    mergedStatistics.stream()
      .filter(row -> row.length >= 5 && Arrays.stream(row).noneMatch(Objects::isNull))
      .forEach(row -> {
        int hour = (Integer) row[0];
        long totalProcesses = (Long) row[1];
        long totalRecords = (Long) row[2];
        long totalProcessingTime = (Long) row[3];
        String fetchType = (String) row[4];

        BulkProcessEstimationResponseDTO estimationResponseDTO =
          new BulkProcessEstimationResponseDTO();
        estimationResponseDTO.setHour(hour);
        estimationResponseDTO.setTotalProcesses(totalProcesses);
        estimationResponseDTO.setTotalRecords(totalRecords);
        estimationResponseDTO.setTotalProcessingTime(totalProcessingTime);
        estimationResponseDTO.setProcessType(processType);
        estimationResponseDTO.setFetchType(fetchType);

        fetchTypeEstimations.computeIfAbsent(fetchType, k -> new ArrayList<>())
          .add(estimationResponseDTO);
      });
    processTypeXEstimations.put(processType, fetchTypeEstimations);
    return processTypeXEstimations;
  }

  public List<String> getSupportedProcessTypesForEstimateEvaluation() {
    return Stream.of(StringUtils.split(supportedProcessTypeForEstimations, ","))
      .collect(Collectors.toList());
  }

  public void updateOrCreateByProcessTypeForBulkProcessEstimations(String processType,
    String deltaTimeEstimations, Date lastFetchTime, Boolean processLevelFetch) {
    BulkProcessDataEstimation savedBulkProcessDataEstimation =
      bulkDataEstimationRepository.findFirstByProcessTypeAndProcessLevelFetch(processType,
        processLevelFetch);
    log.info("Updating the bulk process data estimations for process-type : {} and "
      + "processLevelFetch : {} ", processType, processLevelFetch);

    if (Objects.nonNull(savedBulkProcessDataEstimation)) {
      // Entry exists, update it
      BulkProcessDataEstimation bulkProcessDataEstimation = savedBulkProcessDataEstimation;
      bulkProcessDataEstimation.setDeltaTimeEstimations(deltaTimeEstimations);
      bulkProcessDataEstimation.setLastFetchTime(lastFetchTime);
      bulkProcessDataEstimation.setUpdatedDate(new Date());
      bulkProcessDataEstimation.setId(savedBulkProcessDataEstimation.getId());
      bulkDataEstimationRepository.save(bulkProcessDataEstimation);
    } else {
      // Entry doesn't exist, create a new one
      BulkProcessDataEstimation processDataEstimation = new BulkProcessDataEstimation();
      processDataEstimation.setProcessType(processType);
      processDataEstimation.setProcessLevelFetch(processLevelFetch);
      processDataEstimation.setDeltaTimeEstimations(deltaTimeEstimations);
      processDataEstimation.setLastFetchTime(lastFetchTime);
      processDataEstimation.setMarkForDelete(false);

      BulkProcessDataEstimation bulkProcessDataEstimation =
        bulkDataEstimationRepository.save(processDataEstimation);
      log.info("Saved Entity is : {} ", bulkProcessDataEstimation);
    }
  }

}
