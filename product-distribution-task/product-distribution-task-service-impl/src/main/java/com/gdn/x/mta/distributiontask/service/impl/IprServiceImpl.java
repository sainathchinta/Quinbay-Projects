package com.gdn.x.mta.distributiontask.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.pdt.dto.configuration.distribution.IPRActionResponseDto;
import com.gdn.x.mta.distributiontask.dao.api.IprProductSolrCollectionRepository;
import com.gdn.x.mta.distributiontask.dao.api.IprRepository;
import com.gdn.x.mta.distributiontask.dao.api.IprHistoryRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.model.IPRHistory;
import com.gdn.x.mta.distributiontask.model.ProductIPR;
import com.gdn.x.mta.distributiontask.model.dto.AddingIprProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.BrandReport;
import com.gdn.x.mta.distributiontask.model.dto.IPRHistoryDescription;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.ProductSkuDetailResponse;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import com.gdn.x.mta.distributiontask.model.enums.IPRHistoryActivity;
import com.gdn.x.mta.distributiontask.model.enums.ProductSourceIPR;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import com.gdn.x.mta.distributiontask.rest.model.response.IPRHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductDetailsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprSuspensionInProgressResponse;
import com.gdn.x.mta.distributiontask.service.api.IprService;
import com.gdn.x.mta.distributiontask.service.impl.config.IprSourceConfig;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.service.impl.util.ValidationUtil;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.MDC;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

@Service
@RequiredArgsConstructor
@Slf4j
public class IprServiceImpl implements IprService {

  @Value("${product.change.event.types}")
  private String productChangeEventTypes;

  @Value("${fetch.assignee.from.db}")
  private boolean fetchAssigneeFromDb;

  @Value("${suspend.after.working.days}")
  private boolean suspendAfterWorkingDays;

  private final IprRepository iprRepository;
  private final ProductServiceRepository productServiceRepository;

  private final IprProductSolrCollectionRepository iprProductSolrCollectionRepository;
  private final KafkaPublisher kafkaProducer;
  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private final ObjectMapper objectMapper;

  private final IprHistoryRepository iprHistoryRepository;
  private final IprSourceConfig iprSourceConfig;

  @Override
  public Page<IprSuspensionInProgressResponse> findSuspensionInProgressProducts(String storeId,
      String businessPartnerCode, int page, int size, String sortOrder) throws Exception {
    Page<ProductIPR> productIPR;
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_EMPTY);
    checkArgument(StringUtils.isNotBlank(businessPartnerCode),
        ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    List<String> state = new ArrayList<>();
    state.add(ProductStateIPR.EVIDENCE_REQUESTED.name());
    state.add(ProductStateIPR.EVIDENCE_SUBMITTED.name());
    if (Constants.ASC.equalsIgnoreCase(sortOrder)) {
      productIPR =
        iprRepository.findByStoreIdAndBusinessPartnerCodeAndStateInAndMarkForDeleteFalse(storeId,
          businessPartnerCode, state, PageRequest.of(page, size,
            Sort.by(Sort.Direction.ASC, Constants.EVIDENCE_REQUESTED_DATE)));
    } else {
      productIPR =
        iprRepository.findByStoreIdAndBusinessPartnerCodeAndStateInAndMarkForDeleteFalse(storeId,
          businessPartnerCode, state, PageRequest.of(page, size,
            Sort.by(Sort.Direction.DESC, Constants.EVIDENCE_REQUESTED_DATE)));
    }
    List<IprSuspensionInProgressResponse> responses =
        productIPR.stream().map(IprServiceImpl::mapProductIPRToResponse)
            .collect(Collectors.toList());

    return new PageImpl<>(responses, PageRequest.of(page, size), productIPR.getTotalElements());
  }

  @Override
  public Page<IprProductListResponse> getIprProductListResponse(String storeId,
    IPRProductListRequest iprProductListRequest, Pageable pageable) throws Exception {
    Page<IPRProductSolr> iprProductSolrPage =
      iprProductSolrCollectionRepository.getIprProductsList(storeId, iprProductListRequest,
        pageable);
    List<IPRProductSolr> iprProductSolrList = new ArrayList<>(iprProductSolrPage.getContent());
    if (fetchAssigneeFromDb) {
      List<String> productSkuList =
        Optional.of(iprProductSolrPage.getContent()).orElseGet(Collections::emptyList).stream()
          .map(IPRProductSolr::getProductSku).distinct().collect(Collectors.toList());
      Map<String, String> productSkuAssigeeMap = fetchAssigneeFromDb(productSkuList);
      iprProductSolrList =
        ConverterUtil.populateAssigneeToIprFromDb(iprProductSolrPage.getContent(),
          productSkuAssigeeMap);
    }
    return new PageImpl<>(ConverterUtil.toIprProductListResponses(iprProductSolrList), pageable,
      iprProductSolrPage.getTotalElements());
  }

  @Override
  @Transactional
  public Pair<ProductIPR, IPRHistoryEventModel> updateProductOnStateChange(
      ProductChange productChangeEventModel)
      throws Exception {
    Set<String> validProductChangeTypes =
      productChangeEventModel.getProductChangeEventType().stream()
        .filter(productChangeEventType -> productChangeEventTypes.contains(productChangeEventType))
        .collect(Collectors.toSet());
    IPRHistoryEventModel iprHistoryEventModel = null;
    if (CollectionUtils.isEmpty(validProductChangeTypes)) {
      return Pair.of(null, iprHistoryEventModel);
    }
    ProductIPR existingProductIPR=null;
    if (validProductChangeTypes.contains(Constants.PRODUCT_REJECTED)) {
      existingProductIPR = findByProductSku(productChangeEventModel.getProductSku());
    } else {
      existingProductIPR =
        findByProductSkuAndMarkForDelete(productChangeEventModel.getProductSku(), false);
    }
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        Objects.isNull(existingProductIPR) ?
            Constants.X_PRODUCT_USERNAME :
            existingProductIPR.getUpdatedBy());
    if (Objects.nonNull(existingProductIPR)) {
      IPRHistoryDescription iprHistoryDescription =
          IPRHistoryDescription.builder().previous(existingProductIPR.getState()).build();
      updateProductStateInIpr(productChangeEventModel, validProductChangeTypes, existingProductIPR);
      existingProductIPR = iprRepository.save(existingProductIPR);
      iprHistoryDescription.setCurrent(existingProductIPR.getState());
      if(!iprHistoryDescription.getPrevious().equals(iprHistoryDescription.getCurrent())) {
        iprHistoryEventModel = constructIprHistoryEventModel(existingProductIPR.getProductSku(),
            objectMapper.writeValueAsString(iprHistoryDescription),
            IPRHistoryActivity.STATUS_UPDATE.getValue());
      }
    }
    return Pair.of(existingProductIPR, iprHistoryEventModel);
  }

  private static void updateProductStateInIpr(ProductChange productChangeEventModel,
    Set<String> validProductChangeTypes, ProductIPR existingProductIPR) {
    if (validProductChangeTypes.contains(Constants.FORCE_REVIEW_FLAG_CHANGE)) {
      log.info("Product has changed type - {} ", Constants.FORCE_REVIEW_FLAG_CHANGE);
      if (productChangeEventModel.isMarkForDelete()) {
        existingProductIPR.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
      } else {
        existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
      }
    } else {
      log.info("Product has changed type - {} ", Constants.PRODUCT_REJECTED);
      existingProductIPR.setState(ProductStateIPR.REJECTED.name());
      existingProductIPR.setMarkForDelete(true);
    }
  }

  @Override
  public ProductIPR findByProductSku(String productSku) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    return iprRepository.findByProductSku(productSku);
  }

  @Override
  public ProductIPR findByProductSkuAndMarkForDelete(String productSku, boolean markForDelete) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    return iprRepository.findByProductSkuAndMarkForDelete(productSku, markForDelete);
  }

  private static IprSuspensionInProgressResponse mapProductIPRToResponse(ProductIPR productIPR) {
    IprSuspensionInProgressResponse response = new IprSuspensionInProgressResponse();
    BeanUtils.copyProperties(productIPR, response);
    response.setEvidenceRequestedReasons(productIPR.getReasons());
    response.setEvidenceRequestedNotes(productIPR.getSellerNotes());
    response.setBrand(productIPR.getBrandName());
    response.setProductMainImage(productIPR.getImageUrl());
    response.setEvidenceFilePath(Arrays.asList(
        (Optional.ofNullable(productIPR.getEvidenceFilePath())
            .map(evidenceFilePath -> evidenceFilePath.split(Constants.COMMA))
            .orElseGet(() -> new String[0]))));
    response.setEvidenceUrl(Arrays.asList((Optional.ofNullable(productIPR.getEvidenceUrl())
        .map(evidenceUrl -> evidenceUrl.split(Constants.COMMA))
        .orElseGet(() -> new String[0]))));
    return response;
  }

  @Override
  @Transactional
  public AddingIprProductDTO addProductToIPR(String productSku, String storeId, String source,
      String assignee, BrandReport brandReport) {
    ProductIPR existingProductIPR = iprRepository.findByProductSku(productSku);
    ProductSkuDetailResponse productSkuDetailResponse;
    if (Objects.nonNull(existingProductIPR)) {
      String existingState = existingProductIPR.getState();
      if (stateEligibleToAdd(existingState)) {
        productSkuDetailResponse =
            productServiceRepository.getProductDetailForProduct(productSku, null);
      } else if (ProductStateIPR.IN_REVIEW.name().equals(existingProductIPR.getState())) {
        if (StringUtils.isNotBlank(assignee)) {
          String existingAssignee = existingProductIPR.getAssignedTo();
          existingProductIPR.setAssignedTo(assignee);
          existingProductIPR.setAssignedDate(new Date());
          mergeSource(existingProductIPR, source, brandReport);
          iprRepository.save(existingProductIPR);
          AddingIprProductDTO iprProductDTO = setIprProductDto(ConverterUtil.convertToIPRProductSolr(existingProductIPR),
              ErrorCategory.EMPTY_ERROR_MESSAGE);
          iprProductDTO.setOnlyAssigneeUpdated(true);
          iprProductDTO.setExistingAssignee(existingAssignee);
          return iprProductDTO;
        } else {
          mergeSource(existingProductIPR, source, brandReport);
          iprRepository.save(existingProductIPR);
          return setIprProductDto(ConverterUtil.convertToIPRProductSolr(existingProductIPR),
              ErrorCategory.EMPTY_ERROR_MESSAGE);
        }
      }
      else {
        log.info("product sku: {} already exist with state {}", productSku, existingState);
        return setIprProductDto(null, getErrorMessageForIPR(existingState));
      }
    } else {
      productSkuDetailResponse =
          productServiceRepository.getProductDetailForProduct(productSku, null);
    }
    productSkuDetailResponse.setStoreId(storeId);
    AddingIprProductDTO iprProductDTO = validateProduct(productSku, productSkuDetailResponse);
    if (StringUtils.isNotEmpty(iprProductDTO.getErrorCategory().getMessage())) {
      return iprProductDTO;
    }
    return addIprProducts(existingProductIPR, productSkuDetailResponse, source, assignee, brandReport);
  }

  private static boolean stateEligibleToAdd(String existingState) {
    return existingState.equals(ProductStateIPR.RELEASED.name()) || existingState.equals(
        ProductStateIPR.SUSPENDED.name());
  }

  private AddingIprProductDTO validateProduct(String productSku,
      ProductSkuDetailResponse productSkuDetailResponse) {
    if (productSkuDetailResponse.isRejected()) {
      log.info("Product sku: {} is rejected", productSku);
      return setIprProductDto(null, ErrorCategory.PRODUCT_REJECTED_FROM_INTERNAL);
    }
    if (productSkuDetailResponse.isSuspended()) {
      log.info("Product sku: {} is suspended", productSku);
      return setIprProductDto(null, ErrorCategory.PRODUCT_SUSPENDED_FROM_INTERNAL);
    }
    if (productSkuDetailResponse.isPrelive()) {
      log.info("Product sku: {} is pre-live", productSku);
      return setIprProductDto(null, ErrorCategory.PRODUCT_IS_PRELIVE);
    }
    return setIprProductDto(null, ErrorCategory.EMPTY_ERROR_MESSAGE);
  }

  private AddingIprProductDTO addIprProducts(ProductIPR existingProductIPR,
      ProductSkuDetailResponse productSkuDetailResponse, String source, String assignee,
      BrandReport brandReport) {
    boolean takenDown = productSkuDetailResponse.isMarkForDelete();
    if (Objects.isNull(existingProductIPR)) {
      existingProductIPR = new ProductIPR();
      BeanUtils.copyProperties(productSkuDetailResponse, existingProductIPR);
    }
    updateProductIPR(existingProductIPR, takenDown);
    if (!existingProductIPR.getState().equals(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name())
        && StringUtils.isNotBlank(assignee)) {
      existingProductIPR.setAssignedTo(assignee);
      existingProductIPR.setAssignedDate(new Date());
    }
    String existingSource = existingProductIPR.getSource();
    if (StringUtils.isNotBlank(existingSource)) {
      Set<String> existingSourceSet =
          Arrays.stream(existingSource.split(Constants.COMMA)).collect(Collectors.toSet());
      existingSourceSet.add(getSourceValueOrEmpty(source));
      String updatedSources = String.join(Constants.COMMA, existingSourceSet);
      existingProductIPR.setSource(updatedSources);
    } else {
      existingProductIPR.setSource(getSourceValueOrEmpty(source));
    }
    if(ProductSourceIPR.BRAND_REPORT.getValue().equals(source) && Objects.nonNull(brandReport)) {
      existingProductIPR.setReportDate(brandReport.getReportDate());
      existingProductIPR.setReporter(brandReport.getReporter());
      existingProductIPR.setReporterName(brandReport.getReporterName());
      existingProductIPR.setReporterEmail(brandReport.getReporterEmail());
      existingProductIPR.setReporterReason(brandReport.getReporterReason());
      existingProductIPR.setReporterAddress(brandReport.getReporterAddress());
      existingProductIPR.setReporterPhoneNumber(brandReport.getReporterPhone());
    }
    existingProductIPR = iprRepository.save(existingProductIPR);
    if (!existingProductIPR.getState().equals(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name())) {
      return setIprProductDto(ConverterUtil.convertToIPRProductSolr(existingProductIPR),
          ErrorCategory.EMPTY_ERROR_MESSAGE);
    } else {
      return setIprProductDto(null, ErrorCategory.PRODUCT_WAITING_TO_GET_ACTIVATED);
    }
  }

  private void mergeSource(ProductIPR productIPR, String source, BrandReport brandReport) {
    String sourceStringValue = getSourceValueOrEmpty(source);
    if (StringUtils.isNotBlank(sourceStringValue)) {
      String existingSource = productIPR.getSource();
      if (StringUtils.isNotBlank(existingSource)) {
        LinkedHashSet<String> existingSourceSet = Arrays.stream(existingSource.split(Constants.COMMA))
            .map(String::trim)
            .filter(StringUtils::isNotBlank)
            .collect(Collectors.toCollection(LinkedHashSet::new));
        existingSourceSet.add(sourceStringValue);
        productIPR.setSource(String.join(Constants.COMMA, existingSourceSet));
      } else {
        productIPR.setSource(sourceStringValue);
      }
    }
    if (ProductSourceIPR.BRAND_REPORT.getValue().equals(source) && Objects.nonNull(brandReport)) {
      productIPR.setReportDate(brandReport.getReportDate());
      productIPR.setReporter(brandReport.getReporter());
      productIPR.setReporterName(brandReport.getReporterName());
      productIPR.setReporterEmail(brandReport.getReporterEmail());
      productIPR.setReporterReason(brandReport.getReporterReason());
      productIPR.setReporterAddress(brandReport.getReporterAddress());
      productIPR.setReporterPhoneNumber(brandReport.getReporterPhone());
    }
  }

  private AddingIprProductDTO setIprProductDto(IPRProductSolr iprProductSolr,
      ErrorCategory errorMessage) {
    AddingIprProductDTO addingIprProductDTO = new AddingIprProductDTO();
    addingIprProductDTO.setErrorCategory(errorMessage);
    addingIprProductDTO.setIprProductSolr(iprProductSolr);
    return addingIprProductDTO;
  }

  @Override
  @Transactional
  public Pair<ProductIPR, IPRHistoryEventModel> submitEvidenceForProduct(
      SubmitEvidenceRequest submitEvidenceRequest)
      throws JsonProcessingException {
    ProductIPR existingProductIpr =
      iprRepository.findByProductSku(submitEvidenceRequest.getProductSku());
    ValidationUtil.checkParameter(Objects.nonNull(existingProductIpr), ErrorMessages.PRODUCT_NOT_FOUND_IN_IPR);
    ValidationUtil.checkParameter(
      StringUtils.equals(existingProductIpr.getState(), ProductStateIPR.EVIDENCE_REQUESTED.name()),
      ErrorMessages.INVALID_STATE_TO_SUBMIT_EVIDENCE);
    IPRHistoryEventModel iprHistoryEventModel =
        constructIprHistoryEventModel(existingProductIpr.getProductSku(),
            objectMapper.writeValueAsString(
                IPRHistoryDescription.builder().previous(ProductStateIPR.EVIDENCE_REQUESTED.name())
                    .current(ProductStateIPR.EVIDENCE_SUBMITTED.name()).build()),
            IPRHistoryActivity.STATUS_UPDATE.getValue());
    existingProductIpr.setEvidenceFilePath(
        String.join(Constants.COMMA, submitEvidenceRequest.getEvidenceFilePath()));
    existingProductIpr.setEvidenceSubmittedNotes(submitEvidenceRequest.getEvidenceSubmittedNotes());
    existingProductIpr.setEvidenceUrl(
        String.join(Constants.COMMA, submitEvidenceRequest.getEvidenceUrl()));
    existingProductIpr.setState(ProductStateIPR.EVIDENCE_SUBMITTED.name());
    existingProductIpr.setEvidenceSubmittedBy(GdnMandatoryRequestParameterUtil.getUsername());
    existingProductIpr.setAssignedDate(new Date());
    return Pair.of(iprRepository.save(existingProductIpr), iprHistoryEventModel);
  }

  @Override
  @Transactional
  public IPRActionResponseDto performIprActionForProduct(
      IprActionRequest iprActionRequest, String storeId) throws Exception {
    ProductStateIPR actionState = ProductStateIPR.valueOf(iprActionRequest.getAction());
    ProductIPR existingProductIpr =
        iprRepository.findByProductSku(iprActionRequest.getProductSku());
    IPRHistoryEventModel iprHistoryEventModel = null;
    List<IPRHistoryEventModel> iprHistoryEventModelList = new ArrayList<>();
    String existingState =
      Objects.nonNull(existingProductIpr) ? existingProductIpr.getState() : StringUtils.EMPTY;
    if (Objects.isNull(existingProductIpr)) {
      ValidationUtil.checkParameter(iprActionRequest.isBulkAction(),
          ErrorCategory.PRODUCT_SKU_NOT_FOUND.getCode(),
          ErrorCategory.PRODUCT_SKU_NOT_FOUND.getMessage());
      ProductSkuDetailResponse productSkuDetailResponse =
          productServiceRepository.getProductDetailForProduct(iprActionRequest.getProductSku(),
              null);
      AddingIprProductDTO iprProductDTO =
          validateProduct(iprActionRequest.getProductSku(), productSkuDetailResponse);
      if (StringUtils.isNotEmpty(iprProductDTO.getErrorCategory().getMessage())) {
        return IPRActionResponseDto.builder().errorCategory(iprProductDTO.getErrorCategory())
          .iprHistoryEventModels(iprHistoryEventModelList).build();
      }
      existingProductIpr = new ProductIPR();
      BeanUtils.copyProperties(productSkuDetailResponse, existingProductIpr);
      if (productSkuDetailResponse.isMarkForDelete()) {
        return IPRActionResponseDto.builder()
          .errorCategory(processInactiveProducts(iprActionRequest, existingProductIpr))
          .iprHistoryEventModels(iprHistoryEventModelList).build();
      }
      existingProductIpr.setSource(iprActionRequest.getSource());
      if (ProductSourceIPR.BRAND_REPORT.getValue().equals(iprActionRequest.getSource())
          && Objects.nonNull(iprActionRequest.getBrandReport())) {
        existingProductIpr.setReportDate(iprActionRequest.getBrandReport().getReportDate());
        existingProductIpr.setReporter(iprActionRequest.getBrandReport().getReporter());
        existingProductIpr.setReporterName(iprActionRequest.getBrandReport().getReporterName());
        existingProductIpr.setReporterEmail(iprActionRequest.getBrandReport().getReporterEmail());
        existingProductIpr.setReporterReason(iprActionRequest.getBrandReport().getReporterReason());
        existingProductIpr.setReporterAddress(
            iprActionRequest.getBrandReport().getReporterAddress());
        existingProductIpr.setReporterPhoneNumber(
            iprActionRequest.getBrandReport().getReporterPhone());
      }
      existingProductIpr.setStoreId(storeId);
      existingProductIpr.setProductAddedDate(new Date());
    } else if (stateEligibleToAdd(existingProductIpr.getState())) {
      ValidationUtil.checkParameter(iprActionRequest.isBulkAction(),
          ErrorCategory.PRODUCT_SKU_NOT_FOUND.getCode(),
          ErrorCategory.PRODUCT_SKU_NOT_FOUND.getMessage());
      ProductSkuDetailResponse productSkuDetailResponse =
          productServiceRepository.getProductDetailForProduct(iprActionRequest.getProductSku(),
              null);
      AddingIprProductDTO addingIprProductDTO =
          validateProduct(iprActionRequest.getProductSku(), productSkuDetailResponse);
      if (StringUtils.isNotEmpty(addingIprProductDTO.getErrorCategory().getMessage())) {
        return IPRActionResponseDto.builder().errorCategory(addingIprProductDTO.getErrorCategory())
          .iprHistoryEventModels(iprHistoryEventModelList).build();
      }
      if (productSkuDetailResponse.isMarkForDelete()) {
        return IPRActionResponseDto.builder()
          .errorCategory(processInactiveProducts(iprActionRequest, existingProductIpr))
          .iprHistoryEventModels(iprHistoryEventModelList).build();
      }
    }
    validateAssignee(iprActionRequest, existingProductIpr);
    ErrorCategory messagesForAction =
        getErrorMessagesForAction(existingProductIpr.getState(), actionState);
    if (StringUtils.isNotEmpty(messagesForAction.getMessage())) {
      return IPRActionResponseDto.builder().errorCategory(messagesForAction)
        .iprHistoryEventModels(iprHistoryEventModelList).build();
    }

    IPRHistoryDescription iprHistoryDescription = IPRHistoryDescription.builder().previous(
        StringUtils.isBlank(existingProductIpr.getState()) ?
            ProductStateIPR.IN_REVIEW.name() :
            existingProductIpr.getState()).build();

    processProductIprState(iprActionRequest, existingProductIpr, actionState);

    iprHistoryDescription.setCurrent(existingProductIpr.getState());

    IPRHistoryEventModel iprHistoryEventModelForAssigneeUpdate;

    if (iprActionRequest.isBulkAction() && !Objects.equals(existingProductIpr.getAssignedTo(),
        iprActionRequest.getAssignee())) {
      IPRHistoryDescription iprHistoryDescriptionAssigneeUpdate =
          IPRHistoryDescription.builder().previous(existingProductIpr.getAssignedTo())
              .current(iprActionRequest.getAssignee()).build();
      iprHistoryEventModelForAssigneeUpdate =
          constructIprHistoryEventModel(iprActionRequest.getProductSku(),
              objectMapper.writeValueAsString(iprHistoryDescriptionAssigneeUpdate),
              IPRHistoryActivity.ASSIGNEE_UPDATE.getValue());
      iprHistoryEventModelList.add(iprHistoryEventModelForAssigneeUpdate);
      existingProductIpr.setAssignedTo(iprActionRequest.getAssignee());
      existingProductIpr.setAssignedDate(new Date());
    }

    ProductEmailEventModel productEmailEventModel =
      ConverterUtil.processEventForProductEmail(existingState, existingProductIpr.getState(),
        existingProductIpr);
    iprRepository.save(existingProductIpr);

    if (!iprHistoryDescription.getPrevious().equals(iprHistoryDescription.getCurrent())) {
      iprHistoryEventModel = constructIprHistoryEventModel(iprActionRequest.getProductSku(),
          objectMapper.writeValueAsString(iprHistoryDescription),
          IPRHistoryActivity.STATUS_UPDATE.getValue());
      iprHistoryEventModelList.add(iprHistoryEventModel);
    }
    return IPRActionResponseDto.builder().errorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE)
        .iprHistoryEventModels(iprHistoryEventModelList)
        .productEmailEventModel(productEmailEventModel).build();
  }

  private static void validateAssignee(IprActionRequest iprActionRequest,
      ProductIPR existingProductIpr) {
    if (!iprActionRequest.isBulkAction()) {
      ValidationUtil.checkParameter(StringUtils.isNotEmpty(existingProductIpr.getAssignedTo()),
          ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getCode(),
          ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getMessage());
      ValidationUtil.checkParameter(
          iprActionRequest.getUpdatedBy().equals(existingProductIpr.getAssignedTo()),
          ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getCode(),
          ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getMessage());
    }
  }

  private ErrorCategory processInactiveProducts(IprActionRequest iprActionRequest,
      ProductIPR existingProductIpr) {
    existingProductIpr.setSource(iprActionRequest.getSource());
    existingProductIpr.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    existingProductIpr.setMarkForDelete(false);
    existingProductIpr.setStoreId(iprActionRequest.getStoreId());
    iprRepository.save(existingProductIpr);
    return ErrorCategory.PRODUCT_WAITING_TO_GET_ACTIVATED;
  }

  private void processProductIprState(IprActionRequest iprActionRequest,
      ProductIPR existingProductIpr, ProductStateIPR action) {
    switch (action) {
      case RELEASED:
        processCommonIprFields(existingProductIpr, ProductStateIPR.RELEASED.name(),
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, true);
        break;
      case WHITELISTED:
        processCommonIprFields(existingProductIpr, ProductStateIPR.WHITELISTED.name(),
            StringUtils.EMPTY, iprActionRequest.getSellerNotes(), StringUtils.EMPTY,
            StringUtils.EMPTY, true);
        break;
      case EVIDENCE_REQUESTED:
        processCommonIprFields(existingProductIpr, ProductStateIPR.EVIDENCE_REQUESTED.name(),
            iprActionRequest.getReasons(), iprActionRequest.getSellerNotes(),
            iprActionRequest.getReviewerNotes(), iprActionRequest.getViolationType(), false);
        existingProductIpr.setEvidenceRequestedDate(new Date());
        existingProductIpr.setEvidenceRequestedBy(iprActionRequest.getUpdatedBy());
        existingProductIpr.setEvidenceFilePath(null);
        existingProductIpr.setEvidenceUrl(null);
        existingProductIpr.setViolatedFields(iprActionRequest.getViolatedFields());
        existingProductIpr.setEvidenceSubmittedNotes(null);
        break;
      case SUSPENDED:
        processCommonIprFields(existingProductIpr, ProductStateIPR.SUSPENDED.name(),
            iprActionRequest.getReasons(), iprActionRequest.getSellerNotes(),
            iprActionRequest.getReviewerNotes(), iprActionRequest.getViolationType(), true);
        existingProductIpr.setViolatedFields(iprActionRequest.getViolatedFields());
        productServiceRepository.suspendIprProduct(
            ConverterUtil.convertToSuspensionProductRequest(existingProductIpr, iprActionRequest));
        break;
      default:
        break;
    }
  }

  private void processCommonIprFields(ProductIPR existingProductIpr, String state, String reasons,
      String sellerNotes, String reviewerNotes, String violationType, boolean mfd) {
    existingProductIpr.setState(state);
    existingProductIpr.setReasons(reasons);
    existingProductIpr.setSellerNotes(sellerNotes);
    existingProductIpr.setViolationType(violationType);
    existingProductIpr.setReviewerNotes(reviewerNotes);
    existingProductIpr.setMarkForDelete(mfd);
  }

  private void updateProductIPR(ProductIPR productIPR, boolean takenDown) {
    if (takenDown) {
      productIPR.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    } else {
      productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    }
    productIPR.setMarkForDelete(false);
    productIPR.setProductAddedDate(new Date());
    productIPR.setAssignedTo(null);
    productIPR.setAssignedDate(null);
  }

  private ErrorCategory getErrorMessageForIPR(String existingState) {
    if (StringUtils.isBlank(existingState)) {
      return ErrorCategory.EMPTY_ERROR_MESSAGE;
    }
    switch (ProductStateIPR.valueOf(existingState)) {
      case WAITING_TO_GET_ACTIVATED: return ErrorCategory.PRODUCT_WAITING_TO_GET_ACTIVATED;
      case EVIDENCE_REQUESTED: return ErrorCategory.PRODUCT_EVIDENCE_REQUESTED;
      case EVIDENCE_SUBMITTED: return ErrorCategory.PRODUCT_EVIDENCE_SUBMITTED;
      case WHITELISTED: return ErrorCategory.PRODUCT_WHITELISTED;
      default: return ErrorCategory.EMPTY_ERROR_MESSAGE;
    }
  }

  @Override
  public IprProductDetailsResponse fetchIprProductDetails(String productSku) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    ProductIPR existingProductIPR =
        iprRepository.findByProductSkuAndMarkForDelete(productSku, false);
    ValidationUtil.checkParameter(Objects.nonNull(existingProductIPR),
        ErrorMessages.PRODUCT_NOT_FOUND_IN_IPR);
    SellerAnalyticsResponse response = productServiceRepository.getSellerAnalyticsResponse(
        existingProductIPR.getBusinessPartnerCode());
    if (Objects.isNull(response)) {
      return ConverterUtil.constructIPRProductDetailResponseWithEmptySellerHistoricalData(
          existingProductIPR);
    }
    return ConverterUtil.constructIPRProductDetailResponse(existingProductIPR, response);
  }

  @Override
  @Transactional
  public Pair<List<ProductIPR>, List<IPRHistoryEventModel>> updateAssignee(
      IPRUpdateAssigneeRequest iprUpdateAssigneeRequest) throws JsonProcessingException {
    List<ProductIPR> productIPRs =
        iprRepository.findByProductSkuIn(iprUpdateAssigneeRequest.getProductSku());
    List<IPRHistoryEventModel> iprHistoryEventModelList = new ArrayList<>();

    ValidationUtil.checkParameter(CollectionUtils.isNotEmpty(productIPRs),
        ErrorCategory.PRODUCT_SKU_NOT_FOUND.getCode(),
        ErrorCategory.PRODUCT_SKU_NOT_FOUND.getMessage());
    for (ProductIPR productIPR : productIPRs) {
      ValidationUtil.checkParameter(checkEligibleStateForAssigneeUpdate(productIPR.getState()),
          ErrorCategory.INVALID_STATE_TO_UPDATE_ASSIGNEE.getCode(),
          ErrorCategory.INVALID_STATE_TO_UPDATE_ASSIGNEE.getMessage());
      if (StringUtils.isBlank(iprUpdateAssigneeRequest.getAssignedTo())) {
        ValidationUtil.checkParameter(StringUtils.isNotBlank(productIPR.getAssignedTo()),
            ErrorCategory.IPR_PRODUCT_IS_ALREADY_UNASSIGNED.getCode(),
            ErrorCategory.IPR_PRODUCT_IS_ALREADY_UNASSIGNED.getMessage());
      }

      IPRHistoryEventModel iprHistoryEventModel = null;
      if (!StringUtils.equals(productIPR.getAssignedTo(),
          iprUpdateAssigneeRequest.getAssignedTo())) {
        iprHistoryEventModel =
            constructIprHistoryEventModel(productIPR.getProductSku(),
                objectMapper.writeValueAsString(
                    IPRHistoryDescription.builder().previous(productIPR.getAssignedTo())
                        .current(iprUpdateAssigneeRequest.getAssignedTo()).build()),
                IPRHistoryActivity.ASSIGNEE_UPDATE.getValue());
      }

      productIPR.setAssignedTo(iprUpdateAssigneeRequest.getAssignedTo());
      productIPR.setAssignedDate(new Date());
      iprHistoryEventModelList.add(iprHistoryEventModel);
    }

    return Pair.of(iprRepository.saveAll(productIPRs), iprHistoryEventModelList);
  }

  @Override
  public Map<String, Object> getPrimaryFilterCounts(String storeId) throws Exception {
    return iprProductSolrCollectionRepository.getPrimaryFilterCounts(storeId);
  }

  @Override
  @Async
  public void fetchAndSuspendEvidenceRequestedProduct(String storeId, int daysThreshold,
    Pageable pageable) {
    Date dateBefore = getTime(daysThreshold);
    log.info("Fetching evidence requested product before {} ", dateBefore);
    Page<ProductIPR> productIPRS;
    do {
      productIPRS =
        iprRepository.findByStoreIdAndStateAndEvidenceRequestedDateBeforeAndMarkForDeleteFalse(
          storeId, ProductStateIPR.EVIDENCE_REQUESTED.name(), dateBefore, pageable);
      if (CollectionUtils.isNotEmpty(productIPRS.getContent())) {
        productIPRS.getContent().parallelStream().forEach(productIPR -> {
          log.info("Publishing event {} for productSku {}",
            kafkaTopicPropertiesConsumer.getSuspendIprProductEvent(), productIPR.getProductSku());
          kafkaProducer.send(kafkaTopicPropertiesConsumer.getSuspendIprProductEvent(),
            productIPR.getProductSku(), productIPR.getProductSku());
        });
      }
      pageable = pageable.next();
    } while (productIPRS.hasNext());
  }

  private boolean checkEligibleStateForAssigneeUpdate(String state) {
    return StringUtils.equals(ProductStateIPR.IN_REVIEW.name(), state) || StringUtils.equals(
        ProductStateIPR.EVIDENCE_SUBMITTED.name(), state);
  }

  private ErrorCategory getErrorMessagesForAction(String existingState, ProductStateIPR action) {
    if (StringUtils.isBlank(existingState)) {
      return ErrorCategory.EMPTY_ERROR_MESSAGE;
    }
    if (StringUtils.equals(existingState, action.name()) && !ProductStateIPR.SUSPENDED.equals(
        action)) {
      return ErrorCategory.SAME_IPR_STATE;
    }
    switch (ProductStateIPR.valueOf(existingState)) {
      case WHITELISTED:
        return ErrorCategory.PRODUCT_WHITELISTED;
      case REJECTED:
        return ErrorCategory.PRODUCT_REJECTED_FROM_INTERNAL;
      case WAITING_TO_GET_ACTIVATED:
        return ErrorCategory.PRODUCT_WAITING_TO_GET_ACTIVATED;
      default:
        return ErrorCategory.EMPTY_ERROR_MESSAGE;
    }
  }

  private IPRHistoryEventModel constructIprHistoryEventModel(String productSku, String description,
      String activity) {
    return IPRHistoryEventModel.builder().productSku(productSku).description(description)
        .activity(activity).updatedDate(new Date())
        .updatedBy(GdnMandatoryRequestParameterUtil.getUsername()).build();
  }

  public Date getTime(int days) {
    Date now = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(now);
    calendar.set(Calendar.HOUR_OF_DAY, Constants.ZERO);
    calendar.set(Calendar.MINUTE, Constants.ZERO);
    calendar.set(Calendar.SECOND, Constants.ZERO);
    calendar.set(Calendar.MILLISECOND, Constants.ZERO);
    if (suspendAfterWorkingDays) {
      while (days > 0) {
        calendar.add(Calendar.DAY_OF_YEAR, Constants.DECREASE_ONE_DAY);
        int dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK);
        if (dayOfWeek != Calendar.SATURDAY && dayOfWeek != Calendar.SUNDAY) {
          days--;
        }
      }
    } else {
      calendar.add(Calendar.DAY_OF_YEAR, -days);
    }
    return calendar.getTime();
  }

  @Override
  public Page<IPRHistoryResponse> fetchIprHistoryByProductSku(String storeId, String productSku,
      Pageable page) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
        ErrorMessages.STORE_ID_MUST_NOT_BE_EMPTY);
    ValidationUtil.checkParameter(StringUtils.isNotBlank(productSku),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    Page<IPRHistory> iprHistories =
        iprHistoryRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(
            storeId, productSku, page);
    List<IPRHistoryResponse> iprHistoryResponseList =
        iprHistories.getContent().stream().map(this::convertToHistoryResponse)
            .collect(Collectors.toList());
    return new PageImpl<>(iprHistoryResponseList, iprHistories.getPageable(),
        iprHistories.getTotalElements());
  }

  @Override
  @Transactional
  public void updateIprHistoryForProduct(String storeId,
    IPRHistoryEventModel iprHistoryEventModel) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
      iprHistoryEventModel.getUpdatedBy());
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
      ErrorMessages.STORE_ID_MUST_NOT_BE_EMPTY);
    ValidationUtil.checkParameter(StringUtils.isNotBlank(iprHistoryEventModel.getProductSku()),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    ValidationUtil.checkParameter(
      IPRHistoryActivity.isValidActivity(iprHistoryEventModel.getActivity()),
      ErrorMessages.INVALID_IPR_HISTORY_ACTIVITY);
    iprHistoryRepository.save(ConverterUtil.toIprHistory(storeId, iprHistoryEventModel));
  }

  @Override
  @Transactional
  public AddingIprProductDTO addDSModelProductToIPR(String productSku, String storeId,
      String source) {
    ProductIPR existingProductIPR = iprRepository.findByProductSku(productSku);
    ProductSkuDetailResponse productSkuDetailResponse;
    if (Objects.isNull(existingProductIPR) || stateEligibleToAdd(existingProductIPR.getState())) {
      productSkuDetailResponse =
        productServiceRepository.getProductDetailForProduct(productSku, null);
    } else if (ProductStateIPR.IN_REVIEW.name().equals(existingProductIPR.getState())) {
      mergeSource(existingProductIPR, source, null);
      iprRepository.save(existingProductIPR);
      return new AddingIprProductDTO();
    } else {
      return new AddingIprProductDTO();
    }
    productSkuDetailResponse.setStoreId(storeId);
    AddingIprProductDTO iprProductDTO = validateProduct(productSku, productSkuDetailResponse);
    return StringUtils.isNotEmpty(iprProductDTO.getErrorCategory().getMessage()) ?
        iprProductDTO :
        addIprProducts(existingProductIPR, productSkuDetailResponse, source, null, null);
  }

  private IPRHistoryResponse convertToHistoryResponse(IPRHistory iprHistory) {
    IPRHistoryResponse iprHistoryResponse =
        IPRHistoryResponse.builder().productSku(iprHistory.getProductSku())
            .activity(iprHistory.getActivity()).description(iprHistory.getDescription()).build();
    iprHistoryResponse.setUpdatedBy(iprHistory.getUpdatedBy());
    iprHistoryResponse.setUpdatedDate(iprHistory.getUpdatedDate());
    return iprHistoryResponse;
  }

  private Map<String, String> fetchAssigneeFromDb(List<String> productSku) {
    if (CollectionUtils.isNotEmpty(productSku)) {
      return iprRepository.findByProductSkuIn(productSku).stream().collect(
        Collectors.toMap(ProductIPR::getProductSku,
          ipr -> Optional.ofNullable(ipr.getAssignedTo()).orElse(StringUtils.EMPTY)));
    } else {
      return Collections.emptyMap();
    }
  }

  private String getSourceValueOrEmpty(String source) {
    return iprSourceConfig.sourceNameMap.getOrDefault(source, StringUtils.EMPTY);
  }
}
