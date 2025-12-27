package com.gdn.mta.product.service;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TimeZone;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.MarginRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.valueobject.Margin;
import com.gdn.mta.product.valueobject.OrderItem;
import com.gdn.mta.product.valueobject.OrderItemMarginsResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.mailEvent.CategoryChangeMailEvent;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailDomainEvent;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEvents;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3ItemWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.mta.product.valueobject.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pbp.outbound.margin.feign.MarginFeign;
import com.gdn.partners.pbp.repository.mailEvent.ProductMailEventsRepository;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3WipRepository;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.newrelic.api.agent.Trace;

@Service
public class ProductMailEventServiceImpl implements ProductMailEventService {

  public static final String SPECIAL_MARGIN = "SPECIAL-MARGIN";
  public static final String BASE = "BASE";
  public static final String BASE_ADDON = "BASE-ADDON";
  public static final String BASE_MARGIN = "BASE-MARGIN";
  public static final String PERCENTAGE = "PERCENTAGE";
  public static final String ORDER_ITEM_ID = "-";
  private Logger log = LoggerFactory.getLogger(ProductMailEventService.class);

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private ProductMailEventsRepository productMailEventsRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private MarginRepository marginRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductLevel3WipRepository productLevel3WipRepository;

  @Autowired
  private ProductMailEventPublisher productMailEventPublisher;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private MarginFeign marginFeign;

  @Value("${margin.new.filter.api.enabled}")
  private boolean marginNewFilterApiEnabled;

  @Value("${set.default.orderType.margin}")
  private boolean setDefaultOrderTypeForMargin;

  @Value("${default.orderType.margin}")
  private String defaultOrderTypeForMargin;

  @Value("${delete.mail.event.batch.size}")
  private int deleteMailEventBatchSize;

  private static final String POST_LIVE_ACTIVE_PRODUCTS_MAIL_ERROR =
      "Can not send post live active products mail for business partner: {} ";
  private static final String POST_LIVE_REJECT_PRODUCTS_MAIL_ERROR =
      "Can not send post live reject products mail for business partner: {} ";
  private static final String LANGUAGE_IN = "-in";
  private static final String LANGUAGE_EN = "-en";

  @Trace(dispatcher = true)
  @Override
  public void sendDomainEventForSentForCorrection(String productCode,
      String notes) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductLevel3Wip> productLevel3Wips =
        this.productLevel3WipService.findProductL3WipByStoreIdAndProductCode(storeId, productCode);
    for(ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
      ProfileResponse profileResponse =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(productLevel3Wip.getBusinessPartnerCode());
      String language = profileResponse.getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN;
      SimpleDateFormat format = new SimpleDateFormat("dd MMMM yyyy, hh:mm:ss z", Locale.forLanguageTag("in_ID"));
      format.setTimeZone(TimeZone.getTimeZone("Asia/Jakarta"));
      String createdDate = format.format(productLevel3Wip.getCreatedDate());
      List<List<String>> productDatas = new ArrayList<>();
      List<String> productData = new ArrayList<>();
      productData.add(productLevel3Wip.getProductSku());
      productDatas.add(productData);
      ProductMailDomainEvent productMailDomainEvent = ProductMailDomainEvent.builder()
          .productDatas(productDatas).notes(notes).createdOn(createdDate)
          .merchantCode(productLevel3Wip.getBusinessPartnerCode()).productName(productLevel3Wip.getProductName())
          .productSku(productCode)
          .notificationType(ProductMailEventsEnum.SENT_FOR_CORRECTION.getNotificationType() + language)
          .build();
      if (LANGUAGE_EN.equals(language)) {
        this.productMailEventPublisher.publishProductMailDomainEventSendForCorrectionEn(
            productMailDomainEvent);
      } else {
        productMailEventPublisher.publishProductMailDomainEventSendForCorrection(
            productMailDomainEvent);
      }
    }
  }

  @Trace(dispatcher = true)
  @Override
  public void createAndSaveMailEvent(String productCode, String notes, ProductMailEventsEnum event)
      throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductLevel3Wip> productLevel3Wips =
        this.productLevel3WipService.findProductL3WipByStoreIdAndProductCode(storeId, productCode);
    if (CollectionUtils.isNotEmpty(productLevel3Wips)) {
      for (ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
        ProductMailEvents productMailEvents = ProductMailEvents.builder()
          .events(event).businessPartnerCode(productLevel3Wip.getBusinessPartnerCode())
          .notes(notes).productCode(productCode).productSku(productLevel3Wip.getProductSku())
          .build();
        productMailEvents.setStoreId(storeId);
        this.productMailEventsRepository.save(productMailEvents);
        if(ProductMailEventsEnum.REJECTED.equals(event)){
          this.productNotificationService.sendProductRejectNotification(
              productLevel3Wip.getBusinessPartnerCode(), productLevel3Wip.getProductName());
        }
      }
    }
  }

  @Override
  public void createAndSaveCategoryChangeMailEvent(ProductCollection productCollection,
      CategoryChangeMailEvent categoryChangeMailEvent) throws Exception {
    if(!marginNewFilterApiEnabled) {
      String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
      categoryChangeMailEvent.setNewCategoryCode(productCollection.getCategoryCode());
      categoryChangeMailEvent.setNewCategoryName(productCollection.getCategoryName());
      MarginCategoryResponse existingCategoryMargin = marginRepository.getMarginForStoreIdAndCategoryCode(storeId,
          categoryChangeMailEvent.getExistingCategoryCode());
      MarginCategoryResponse newCategoryMargin =
          marginRepository.getMarginForStoreIdAndCategoryCode(storeId, categoryChangeMailEvent.getNewCategoryCode());
      if (Objects.nonNull(newCategoryMargin)) {
        categoryChangeMailEvent.setNewCategoryMargin(String.valueOf(newCategoryMargin.getValue()));
        categoryChangeMailEvent.setNewCategoryValueType(newCategoryMargin.getValueType());
        if (Objects.nonNull(existingCategoryMargin)) {
          categoryChangeMailEvent.setOldCategoryMargin(String.valueOf(existingCategoryMargin.getValue()));
          categoryChangeMailEvent.setOldCategoryValueType(existingCategoryMargin.getValueType());
        } else {
          log.info("Margin is not present for the old category : {}", categoryChangeMailEvent.getExistingCategoryCode());
          categoryChangeMailEvent.setOldCategoryMargin(Constants.STRING_ZERO);
          categoryChangeMailEvent.setOldCategoryValueType(Constants.HYPHEN);
        }
        String notes = objectMapper.writeValueAsString(categoryChangeMailEvent);
        createAndSaveMailEvent(productCollection.getProductCode(), notes, ProductMailEventsEnum.CATEGORY_CHANGE);
      } else {
        log.warn("Margin is not present for the new category : {}", categoryChangeMailEvent.getNewCategoryCode());
      }
    } else {
      FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequestForExisting =
          getMarginsByOrderItemRequest(productCollection.getBusinessPartnerCode(), productCollection.getCategoryCode());
      FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequestForNew =
          getMarginsByOrderItemRequest(productCollection.getBusinessPartnerCode(),
              categoryChangeMailEvent.getNewCategoryCode());

      Optional<OrderItemMarginsResponse> orderItemMarginsResponseForNew =
          getOrderItemMarginsResponse(filterMarginsByOrderItemsRequestForNew);
      Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting =
          getOrderItemMarginsResponse(filterMarginsByOrderItemsRequestForExisting);


      if (orderItemMarginsResponseForNew.isPresent()) {
        getMarginBusinessPartnerWebResponse(orderItemMarginsResponseForNew, categoryChangeMailEvent);
        if (orderItemMarginsResponseForExisting.isPresent()) {
          getMarginBusinessPartnerWebResponseForExisting(orderItemMarginsResponseForExisting, categoryChangeMailEvent);
        } else {
          log.info("Margin is not present for the old category : {}",
              categoryChangeMailEvent.getExistingCategoryCode());
          categoryChangeMailEvent.setOldCategoryMargin(Constants.STRING_ZERO);
          categoryChangeMailEvent.setOldCategoryValueType(Constants.HYPHEN);
        }
        String notes = objectMapper.writeValueAsString(categoryChangeMailEvent);
        createAndSaveMailEvent(productCollection.getProductCode(), notes, ProductMailEventsEnum.CATEGORY_CHANGE);
      } else {
        log.warn("Margin is not present for the new category : {}", categoryChangeMailEvent.getNewCategoryCode());
      }
    }
  }

  public static void getMarginBusinessPartnerWebResponse(Optional<OrderItemMarginsResponse> orderItemMarginsResponse,
      CategoryChangeMailEvent categoryChangeMailEvent) {
    if (Objects.nonNull(orderItemMarginsResponse)) {
      Optional<Margin> firstMargin = orderItemMarginsResponse.flatMap(
          response -> Optional.ofNullable(response.getMargins()).orElse(new ArrayList<>()).stream().filter(Objects::nonNull).findFirst());
      firstMargin.ifPresent(margin -> {
        categoryChangeMailEvent.setNewCategoryValueType(PERCENTAGE);
        categoryChangeMailEvent.setNewCategoryMargin(String.valueOf(margin.getMarginPercentage()));
      });
    }
  }

  public static void getMarginBusinessPartnerWebResponseForExisting(
      Optional<OrderItemMarginsResponse> orderItemMarginsResponse, CategoryChangeMailEvent categoryChangeMailEvent) {
    if (Objects.nonNull(orderItemMarginsResponse)) {
      Optional<Margin> firstMargin = orderItemMarginsResponse.flatMap(
          response -> Optional.ofNullable(response.getMargins()).orElse(new ArrayList<>()).stream().findFirst());
      firstMargin.ifPresent(margin -> {
        categoryChangeMailEvent.setOldCategoryValueType(PERCENTAGE);
        categoryChangeMailEvent.setOldCategoryMargin(String.valueOf(margin.getMarginPercentage()));
      });
    }
  }

  private FilterMarginsByOrderItemsRequest getMarginsByOrderItemRequest(String businessPartnercode,
      String categoryCode) {
    FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequest = new FilterMarginsByOrderItemsRequest();
    OrderItem orderItemForExisting = new OrderItem();
    orderItemForExisting.setCategoryCode(categoryCode);
    orderItemForExisting.setOrderItemId(ORDER_ITEM_ID);
    orderItemForExisting.setTransactionDate(new Date());
    orderItemForExisting.setStoreCode(businessPartnercode);
    if (setDefaultOrderTypeForMargin) {
      orderItemForExisting.setOrderType(defaultOrderTypeForMargin);
    }
    filterMarginsByOrderItemsRequest.setMarginOrderItem(Collections.singletonList(orderItemForExisting));
    return filterMarginsByOrderItemsRequest;
  }

  private Optional<OrderItemMarginsResponse> getOrderItemMarginsResponse(
      FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequest) throws Exception {
    ListBaseResponse<OrderItemMarginsResponse> orderItemMarginsResponseListBaseResponse =
        marginFeign.filterMargin(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            filterMarginsByOrderItemsRequest);
    ResponseHelper.validateResponse(orderItemMarginsResponseListBaseResponse);
    return orderItemMarginsResponseListBaseResponse.getContent().stream().filter(
            orderItemMarginsResponse1 -> Optional.ofNullable(orderItemMarginsResponse1.getMargins())
                .orElse(new ArrayList<>()).stream().filter(Objects::nonNull).anyMatch(margin ->
                    (SPECIAL_MARGIN.equals(margin.getMarginType()) && (BASE.equals(margin.getReplacementType())
                        || BASE_ADDON.equals(margin.getReplacementType()))) || BASE_MARGIN.equals(margin.getMarginType())))
        .findFirst();
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void sendProductMailEventsToBusinessPartners(Date date) throws Exception {
    String storeId = mandatoryParameterHelper.getStoreId();
    List<String> businessPartnerCodes = productMailEventsRepository
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(storeId, date);
    log.info("Business partner codes to send mail {}", businessPartnerCodes);
    for(String businessPartnerCode : businessPartnerCodes){
      log.info("Sending mail for business partner code {} ", businessPartnerCode);
      try {
        ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
        String language = profileResponse.getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN;
        sendRejectProductMail(storeId, businessPartnerCode, date, language);
        sendActiveAndCategoryChangeProductMail(storeId, businessPartnerCode, date, language);
      } catch (Exception ex) {
        log.error("Can not send mail for business partner code {} ", businessPartnerCode, ex);
      }
    }
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void sendPostLiveReviewActiveProductMailEventsToBusinessPartners(Date date) {
    String storeId = mandatoryParameterHelper.getStoreId();
    List<String> businessPartnerCodes = productMailEventsRepository
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(storeId, date);
    for(String businessPartnerCode : businessPartnerCodes){
      try {
        ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
        String language = profileResponse.getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN;
        sendPostLiveReviewActiveProductMail(storeId, businessPartnerCode, date, language);
      } catch (Exception ex) {
        log.error(POST_LIVE_ACTIVE_PRODUCTS_MAIL_ERROR, businessPartnerCode, ex);
      }
    }
  }

  @Trace(dispatcher = true)
  @Async
  @Override
  public void sendPostLiveReviewRejectProductMailEventsToBusinessPartners(Date date) {
    String storeId = mandatoryParameterHelper.getStoreId();
    List<String> businessPartnerCodes =
        productMailEventsRepository.findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(storeId, date);
    for (String businessPartnerCode : businessPartnerCodes) {
      try {
        ProfileResponse profileResponse =
            businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
        String language = profileResponse.getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN;
        sendPostLiveReviewRejectProductMail(storeId, businessPartnerCode, date, language);
      } catch (Exception ex) {
        log.error(POST_LIVE_REJECT_PRODUCTS_MAIL_ERROR, businessPartnerCode, ex);
      }
    }
  }

  private void sendItemSuspensionOrActivationMail(String storeId, String businessPartnerCode, Date date, String language,
      ProductMailEventsEnum event)
      throws Exception {
    log.info("Initiating the email event for {} products for businessPartnerCode : {} ", event, businessPartnerCode);
    List<ProductMailEvents> activeProductEvents = productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(storeId, businessPartnerCode,
            event, date);
    List<List<String>> activeProductDatas = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(activeProductEvents)) {
      for (ProductMailEvents activeProductEvent : activeProductEvents) {
        ProductAndItemsResponse productAndItemsResponse =
            productLevel3Repository.findDetailByProductSkuForSuspension(activeProductEvent.getProductSku());
        List<String> itemSkus = new ArrayList<>();
        for (ItemResponse item : productAndItemsResponse.getItems()) {
          itemSkus.add(item.getItemSku());
          if (itemSkus.size() == 10) {
            break;
          }
        }
        Map<String, String> itemNameBySkus = productLevel3Repository.getItemNameByItemSku(itemSkus);
        for (Map.Entry<String, String> itemData : itemNameBySkus.entrySet()) {
          List<String> productData = new ArrayList<>();
          productData.add(itemData.getKey());
          productData.add(itemData.getValue());
          productData.add(activeProductEvent.getNotes());
          activeProductDatas.add(productData);
        }
        if (activeProductDatas.size() >= 10) {
          activeProductDatas = activeProductDatas.subList(0, 10);
          break;
        }
      }
      publishItemSuspensionOrActivationMailEvent(businessPartnerCode, language, activeProductDatas, event);
      log.info("Successfully published {} products to kafka for business partner code {}", event, businessPartnerCode);
    } else {
      log.info("No active events for suspension/activation to send for date " + date + " businessPartner :" + businessPartnerCode);
    }
  }

  private void publishItemSuspensionOrActivationMailEvent(String businessPartnerCode, String language,
      List<List<String>> activeProductDatas, ProductMailEventsEnum event) {
    log.info("Publishing the email event for {} products for businessPartnerCode : {} ", event, businessPartnerCode);
    ProductMailDomainEvent activeProductMailDomainEvent =
        ProductMailDomainEvent.builder().merchantCode(businessPartnerCode).productDatas(activeProductDatas)
            .notificationType(event.getNotificationType().concat(language)).build();
    if (LANGUAGE_EN.equals(language) && ProductMailEventsEnum.SUSPENDED.equals(event)) {
      this.productMailEventPublisher.publishItemSuspensionMailEventEn(activeProductMailDomainEvent);
    } else if (LANGUAGE_IN.equals(language) && ProductMailEventsEnum.SUSPENDED.equals(event)) {
      this.productMailEventPublisher.publishItemSuspensionMailEvent(activeProductMailDomainEvent);
    } else if (LANGUAGE_EN.equals(language) && ProductMailEventsEnum.RE_ACTIVATED.equals(event)) {
      this.productMailEventPublisher.publishItemReActivationMailEventEn(activeProductMailDomainEvent);
    } else {
      this.productMailEventPublisher.publishItemReActivationMailEvent(activeProductMailDomainEvent);
    }
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void sendMailForArchivedItemSkuDueToOos(Map<String, List<List<String>>> emails,
      Map<String, ProfileResponse> profileResponseMap) throws Exception {
    for (String businessPartnerCode : emails.keySet()) {
      String language =
          profileResponseMap.get(businessPartnerCode).getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN;
      sendItemSkuArchivedMail(emails.get(businessPartnerCode), businessPartnerCode, language);
    }
  }

  private void sendItemSkuArchivedMail(List<List<String>> itemSkuData, String businessPartnerCode, String language) {
    ProductMailDomainEvent productMailDomainEvent = ProductMailDomainEvent.builder().productDatas(itemSkuData).merchantCode(businessPartnerCode)
        .notificationType(ProductMailEventsEnum.AUTO_ARCHIVED.getNotificationType().concat(language)).build();
    if (LANGUAGE_EN.equals(language)) {
      this.productMailEventPublisher.publishItemSkuArchivedEventEn(productMailDomainEvent);
    } else {
      this.productMailEventPublisher.publishItemSkuArchivedEvent(productMailDomainEvent);
    }
  }

  private void sendPostLiveReviewActiveProductMail (
      String storeId, String businessPartnerCode, Date date, String language) throws Exception{
    List<ProductMailEvents> activeProductEvents = productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(storeId,
            businessPartnerCode, ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED, date);
    if (CollectionUtils.isNotEmpty(activeProductEvents)) {
      List<List<String>> postLiveReviewActiveProductsData = new ArrayList<>();
      for (ProductMailEvents activeProductEvent : activeProductEvents) {
        ProductLevel3Wip productLevel3Wip = productLevel3WipService
            .getProductLevel3WipByProductSkuWithItemsInitialised(storeId, activeProductEvent.getProductSku());
        ProductDetailResponse productDetailResponse;
        try {
          productDetailResponse = productRepository.findProductDetailByProductCode(activeProductEvent.getProductCode());
        } catch (Exception e) {
          log.error("Product data not found for merchant : {} and productCode : {} ",
              activeProductEvent.getBusinessPartnerCode(), activeProductEvent.getProductCode(), e);
          continue;
        }
        Map<String, String> idToNameMap = productDetailResponse.getProductItemResponses()
            .stream().collect(Collectors.toMap(ProductItemResponse::getId, ProductItemResponse::getGeneratedItemName));
        SimpleDateFormat EMAIL_DATE_FORMAT = new SimpleDateFormat("dd MMM yyyy hh:mm:ss");
        for (ProductLevel3ItemWip productLevel3ItemWip : productLevel3Wip.getItems()) {
          List<String> productData = new ArrayList<>();
          productData.add(EMAIL_DATE_FORMAT.format(activeProductEvent.getCreatedDate()));
          productData.add(productLevel3ItemWip.getGdnSku());
          productData.add(idToNameMap.get(productLevel3ItemWip.getProductLevel1ItemId()));
          productData.add(productLevel3Wip.getProductSku());
          postLiveReviewActiveProductsData.add(productData);
        }
      }
      publishPostLiveReviewActiveProductsMailEvent(businessPartnerCode, language, postLiveReviewActiveProductsData);
    }else{
      log.info("No post live review active events to send for date: {}  businessPartner : {}", date, businessPartnerCode);
    }
  }

  private void sendPostLiveReviewRejectProductMail(String storeId, String businessPartnerCode, Date date,
      String language) throws Exception {
    List<ProductMailEvents> rejectProductEvents = productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(storeId, businessPartnerCode,
            ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED, date);
    if (CollectionUtils.isNotEmpty(rejectProductEvents)) {
      SimpleDateFormat EMAIL_DATE_FORMAT = new SimpleDateFormat("dd MMM yyyy hh:mm:ss");
      List<List<String>> postLiveReviewRejectProductsData = new ArrayList<>();
      for (ProductMailEvents rejectProductEvent : rejectProductEvents) {
        ProductLevel3Wip productLevel3Wip = productLevel3WipService
            .getProductLevel3WipByProductSkuWithItemsInitialised(storeId, rejectProductEvent.getProductSku());
        ProductDetailResponse productDetailResponse =
            productRepository.findProductDetailByProductCode(rejectProductEvent.getProductCode(), true);
        Map<String, String> idToNameMap = productDetailResponse.getProductItemResponses().stream()
            .collect(Collectors.toMap(ProductItemResponse::getId, ProductItemResponse::getGeneratedItemName));
        for (ProductLevel3ItemWip productLevel3ItemWip : productLevel3Wip.getItems()) {
          List<String> productData = new ArrayList<>();
          productData.add(EMAIL_DATE_FORMAT.format(rejectProductEvent.getCreatedDate()));
          productData.add(productLevel3ItemWip.getGdnSku());
          productData.add(idToNameMap.get(productLevel3ItemWip.getProductLevel1ItemId()));
          productData.add(rejectProductEvent.getNotes());
          postLiveReviewRejectProductsData.add(productData);
        }
      }
      publishPostLiveReviewRejectProductsMailEvent(businessPartnerCode, language, postLiveReviewRejectProductsData);
    } else {
      log.info("No post live review reject events to send for date: {}  businessPartner : {}", date,
          businessPartnerCode);
    }
  }

  private void sendActiveAndCategoryChangeProductMail(String storeId, String businessPartnerCode, Date date,
      String language) throws Exception {
    List<ProductMailEvents> activeProductEvents = productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsInAndCreatedDateGreaterThan(storeId, businessPartnerCode,
            Arrays.asList(ProductMailEventsEnum.APPROVED, ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED), date);
    log.info("Get active and category change mail events for {} , count {}", businessPartnerCode,
        activeProductEvents.size());
    if (CollectionUtils.isNotEmpty(activeProductEvents)) {
      List<List<String>> activeProductDatas = new ArrayList<>();
      List<List<String>> categoryChangeProductDatas = new ArrayList<>();
      for (ProductMailEvents activeProductEvent : activeProductEvents) {
        ProductLevel3Wip productLevel3Wip = productLevel3WipService
            .getProductLevel3WipByProductSkuWithItemsInitialised(storeId, activeProductEvent.getProductSku());
        CategoryChangeMailEvent categoryChangeMailEvent =
            getCategoryChangeMailEvent(storeId, activeProductEvent.getProductSku());
        ProductDetailResponse productDetailResponse = new ProductDetailResponse();
        try {
          productDetailResponse =
              this.productRepository.findProductDetailByProductCode(activeProductEvent.getProductCode());
        } catch (Exception exception) {
          log.error("Error while send ActiveAndCategoryChangeProductMail for : {}, {}",
              activeProductEvent.getProductCode(), exception);
          continue;
        }
        Map<String, String> idToNameMap = productDetailResponse.getProductItemResponses().stream()
            .collect(Collectors.toMap(ProductItemResponse::getId, ProductItemResponse::getGeneratedItemName));
        SimpleDateFormat EMAIL_DATE_FORMAT = new SimpleDateFormat("dd MMM yyyy hh:mm:ss");
        for (ProductLevel3ItemWip productLevel3ItemWip : productLevel3Wip.getItems()) {
          List<String> productData = new ArrayList<>();
          productData.add(EMAIL_DATE_FORMAT.format(activeProductEvent.getCreatedDate()));
          productData.add(productLevel3ItemWip.getGdnSku());
          productData.add(idToNameMap.get(productLevel3ItemWip.getProductLevel1ItemId()));
          productData.add(productLevel3Wip.getProductSku());
          if (ProductMailEventsEnum.APPROVED.equals(activeProductEvent.getEvents())) {
            activeProductDatas.add(productData);
          }

          if (Objects.nonNull(categoryChangeMailEvent)) {
            List<String> categoryData = new ArrayList<>(productData);
            categoryData.add(categoryChangeMailEvent.getExistingCategoryName());
            categoryData.add(categoryChangeMailEvent.getOldCategoryMargin());
            categoryData.add(categoryChangeMailEvent.getNewCategoryName());
            categoryData.add(categoryChangeMailEvent.getNewCategoryMargin());
            categoryChangeProductDatas.add(categoryData);
          }
        }
      }
      publishActiveAndCategoryChangeMailEvent(businessPartnerCode, language, activeProductDatas,
          categoryChangeProductDatas);
      log.info("Successfully published active and category change to kafka for business code {}", businessPartnerCode);
    } else {
      log.info("No active events to send for date " + date + " businessPartner :" + businessPartnerCode);
    }
  }

  private void publishActiveAndCategoryChangeMailEvent(String businessPartnerCode, String language,
      List<List<String>> activeProductDatas, List<List<String>> categoryChangeProductDatas) {
    log.info("Business partner{} , active : {} , category change : {}", businessPartnerCode,
        activeProductDatas.size(), categoryChangeProductDatas.size());
    if(CollectionUtils.isNotEmpty(activeProductDatas)) {
      ProductMailDomainEvent activeProductMailDomainEvent = ProductMailDomainEvent.builder()
          .merchantCode(businessPartnerCode).productDatas(activeProductDatas)
          .notificationType(ProductMailEventsEnum.APPROVED.getNotificationType().concat(language))
          .build();
      if (LANGUAGE_EN.equals(language)) {
        this.productMailEventPublisher.publishProductMailDomainEventActiveEn(
            activeProductMailDomainEvent);
      } else {
        this.productMailEventPublisher.publishProductMailDomainEventActive(
            activeProductMailDomainEvent);
      }
    }
    if(CollectionUtils.isNotEmpty(categoryChangeProductDatas)) {
      ProductMailDomainEvent categoryChangeDomainEvent = ProductMailDomainEvent.builder()
          .merchantCode(businessPartnerCode).productDatas(categoryChangeProductDatas)
          .notificationType(ProductMailEventsEnum.CATEGORY_CHANGE.getNotificationType().concat(language))
          .build();
      if (LANGUAGE_EN.equals(language)) {
        this.productMailEventPublisher.publishProductMailDomainEventCategoryChangeEn(
            categoryChangeDomainEvent);
      } else {
        this.productMailEventPublisher.publishProductMailDomainEventCategoryChange(
            categoryChangeDomainEvent);
      }
    }
  }

  private void publishPostLiveReviewActiveProductsMailEvent(
      String businessPartnerCode, String language, List<List<String>> postLiveActiveProductsData) {
    if(CollectionUtils.isNotEmpty(postLiveActiveProductsData)) {
      ProductMailDomainEvent postLiveReviewActiveProductMailDomainEvent = ProductMailDomainEvent.builder()
          .merchantCode(businessPartnerCode).productDatas(postLiveActiveProductsData)
          .notificationType(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType().concat(language))
          .build();
      if (LANGUAGE_EN.equals(language)) {
        productMailEventPublisher
            .publishPostLiveReviewApprovedProductMailDomainEventEn(postLiveReviewActiveProductMailDomainEvent);
      } else {
        productMailEventPublisher
            .publishPostLiveReviewApprovedProductMailDomainEvent(postLiveReviewActiveProductMailDomainEvent);
      }
    }
  }

  private void publishPostLiveReviewRejectProductsMailEvent(String businessPartnerCode, String language,
      List<List<String>> postLiveRejectProductsData) {
    if (CollectionUtils.isNotEmpty(postLiveRejectProductsData)) {
      ProductMailDomainEvent postLiveReviewRejectProductMailDomainEvent =
          ProductMailDomainEvent.builder().merchantCode(businessPartnerCode).productDatas(postLiveRejectProductsData)
              .notificationType(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED.getNotificationType().concat(language))
              .build();
      if (LANGUAGE_EN.equals(language)) {
        productMailEventPublisher
            .publishPostLiveReviewRejectedProductMailDomainEventEn(postLiveReviewRejectProductMailDomainEvent);
      } else {
        productMailEventPublisher
            .publishPostLiveReviewRejectedProductMailDomainEvent(postLiveReviewRejectProductMailDomainEvent);
      }
    }
  }

  private void sendRejectProductMail(String storeId, String businessPartnerCode, Date date,
      String language) {
    List<ProductMailEvents> rejectedProductEvents = productMailEventsRepository.findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(
        storeId,
            businessPartnerCode,
        ProductMailEventsEnum.REJECTED,
        date);
    if (CollectionUtils.isNotEmpty(rejectedProductEvents)) {
      List<List<String>> productDatas = new ArrayList<>();
      SimpleDateFormat EMAIL_DATE_FORMAT = new SimpleDateFormat("dd MMM yyyy hh:mm:ss");
      for (ProductMailEvents rejectedProductEvent : rejectedProductEvents) {
        ProductLevel3Wip productLevel3Wip = productLevel3WipRepository.findByStoreIdAndProductSku(
            storeId,
            rejectedProductEvent.getProductSku());
        List<String> productData = new ArrayList<>();
        productData.add(EMAIL_DATE_FORMAT.format(productLevel3Wip.getCreatedDate()));
        productData.add(rejectedProductEvent.getProductCode());
        productData.add(productLevel3Wip.getProductName());
        productData.add(rejectedProductEvent.getNotes());
        productDatas.add(productData);
      }
      if (!productDatas.isEmpty()) {
        ProductMailDomainEvent productMailDomainEvent = ProductMailDomainEvent.builder()
            .merchantCode(businessPartnerCode)
            .productDatas(productDatas)
            .notificationType(ProductMailEventsEnum.REJECTED.getNotificationType().concat(language))
            .build();
        if (LANGUAGE_EN.equals(language)) {
          this.productMailEventPublisher.publishProductMailDomainEventRejectedEn(
              productMailDomainEvent);
        } else {
          this.productMailEventPublisher.publishProductMailDomainEventRejected(
              productMailDomainEvent);
        }
        log.info("Successfully published reject event to kafka for business partner", businessPartnerCode);
      }
    }else {
      log.info("No Reject Event to send for date " + date + "businessPartner :" + businessPartnerCode);
    }
  }

  @Override
  public CategoryChangeMailEvent getCategoryChangeMailEvent(String storeId, String gdnSku)
      throws Exception {
    ProductMailEvents initialCategoryChangeEvent = productMailEventsRepository
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(storeId, gdnSku,
            ProductMailEventsEnum.CATEGORY_CHANGE);
    if (Objects.nonNull(initialCategoryChangeEvent)) {
      ProductMailEvents finalCategoryChangeEvent = productMailEventsRepository
          .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateDesc(storeId, gdnSku,
              ProductMailEventsEnum.CATEGORY_CHANGE);
      CategoryChangeMailEvent initialCategoryChangeMailEvent =
          objectMapper.readValue(initialCategoryChangeEvent.getNotes(), CategoryChangeMailEvent.class);
      CategoryChangeMailEvent finalCategoryChangeMailEvent =
          objectMapper.readValue(finalCategoryChangeEvent.getNotes(), CategoryChangeMailEvent.class);
      return CategoryChangeMailEvent.builder()
          .existingCategoryName(initialCategoryChangeMailEvent.getExistingCategoryName())
          .oldCategoryMargin(initialCategoryChangeMailEvent.getOldCategoryMargin())
          .newCategoryName(finalCategoryChangeMailEvent.getNewCategoryName())
          .newCategoryMargin(finalCategoryChangeMailEvent.getNewCategoryMargin()).build();
    }
    return null;
  }

  @Override
  public void createAndSaveMailEventForSuspensionOrReActivation(ProductMailEventsEnum event, String merchantCode, String notes,
      String productCode, String productSku) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductMailEvents productMailEvent = ProductMailEvents.builder()
        .events(event).businessPartnerCode(merchantCode)
        .notes(notes).productCode(productCode).productSku(productSku)
        .build();
    productMailEvent.setStoreId(storeId);
    log.info("Successfully saving the emailEvent for productSku {} with productMailEvent {} ", productSku, productMailEvent);
    this.productMailEventsRepository.save(productMailEvent);
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void sendProductMailEventsToBusinessPartnersForSuspension(Date date) throws Exception {
    String storeId =  mandatoryParameterHelper.getStoreId();
    List<ProductMailEventsEnum> events = Arrays.asList(ProductMailEventsEnum.SUSPENDED, ProductMailEventsEnum.RE_ACTIVATED);
    List<String> businessPartnerCodes =
        productMailEventsRepository.findDistinctBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(storeId, date, events);
    log.info("Business partner codes to send mail for suspension or activation activity {} after date {}",
        businessPartnerCodes, date);
    if (CollectionUtils.isNotEmpty(businessPartnerCodes)) {
      for (String businessPartnerCode : businessPartnerCodes) {
        log.info("Sending mail for business partner code for suspension or activation {} ", businessPartnerCode);
        try {
          ProfileResponse profileResponse =
              businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
          String language = profileResponse.getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN;
          sendItemSuspensionOrActivationMail(storeId, businessPartnerCode, date, language,
              ProductMailEventsEnum.SUSPENDED);
          sendItemSuspensionOrActivationMail(storeId, businessPartnerCode, date, language,
              ProductMailEventsEnum.RE_ACTIVATED);
        } catch (Exception ex) {
          log.error("Can not send mail for business partner code for suspension or activation {} ", businessPartnerCode,
              ex);
        }
      }
    } else {
      log.info("No suspension/activation events for business partners to send");
    }
  }

  @Override
  public void deleteOldRecordsByDays(int days) {
    int totalRecordsDeleted = 0;
    int deletedCount;
    long startTime = System.currentTimeMillis();
    do {
      deletedCount = productMailEventsRepository.deleteOldRecords(days, deleteMailEventBatchSize);
      totalRecordsDeleted += deletedCount;
    } while (deletedCount > 0);
    long endTime = System.currentTimeMillis();
    double totalTimeTakenInMinutes = (endTime - startTime) / (1000.0 * 60.0);
    log.info(
      "Total records deleted from mail event Repository for execution  were : {}, time taken : {} ",
      totalRecordsDeleted, totalTimeTakenInMinutes);

  }
}
