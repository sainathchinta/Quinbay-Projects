package com.gdn.mta.product.service;

import static com.gdn.x.product.domain.event.config.ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY;

import java.lang.reflect.InvocationTargetException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.UpdatedProductHistoryCustomRepository;
import com.gdn.mta.product.service.config.PreOrderConfig;
import lombok.RequiredArgsConstructor;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.safety.Whitelist;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.time.DateUtils;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.repository.AuditTrailUpdateProductBackupRepository;
import com.gdn.mta.product.repository.UpdatedProductHistoryRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.service.converter.UpdateProductItemLevel3ModelConverter;
import com.gdn.mta.product.service.util.LogAuditUpdateProductAnnotationUtil;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.SummaryFilterUtil;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@RequiredArgsConstructor
public class UpdatedProductHistoryServiceBean implements UpdatedProductHistoryService {

  private static final Logger LOGGER = LoggerFactory.getLogger(UpdatedProductHistoryServiceBean.class);

  private static final String PRODUCT_MERCHANT_ACTIVE = "Produk Merchant Aktif";
  private static final String INITIAL_VALUE_OF_STOCK_QUANTITY = "Stok awal";
  private static final String INITIAL_VALUE_OF_PREORDER_QUOTA = "preOrder Quota awal";
  private static final String INITIAL_VALUE_OF_NORMAL_PRICE = "Harga normal awal";
  private static final String INITIAL_VALUE_OF_DISCOUNT_PRICE = "Harga diskon awal";
  private static final String INITIAL_VALUE_OF_BUYABLE_FLAG = "Dapat dibeli";
  private static final String INITIAL_VALUE_OF_DISPLAYABLE_FLAG = "Dapat ditampilkan";
  private static final String DASH_DELIMITER= "-";
  private static final String ATTRIBUTE_MAP = "attributesMap";
  private static final String WHOLESALE_RULES = "wholesaleRules";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String STOCK_VALUE = "availableStockLevel2";
  private static final int MAX_CHARACTER_FOR_AUDIT_VALUE = 255;
  private static final String FORMAT = "################";
  private static final String IFRAME = "iframe";
  public static final String CLIENT_HOST_KEY = "clientHost";
  private static final String DEFAULT_USER = "System";

  private final UpdatedProductHistoryRepository updatedProductHistoryRepository;

  private final UpdatedProductHistoryCustomRepository updatedProductHistoryCustomRepository;

  private final LogAuditUpdateProductAnnotationUtil logAuditUpdateProductAnnotationUtil;

  private  final ProductBusinessPartnerRepository productBusinessPartnerRepository;

  private final UpdateProductItemLevel3ModelConverter updateProductItemLevel3ModelConverter;

  private final AuditTrailUpdateProductBackupRepository auditTrailUpdateProductBackupRepository;

  private final ProductSystemParameterService productSystemParameterService;

  private final SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  private final MandatoryParameterHelper mandatoryParameterHelper;

  private final XProductOutbound xProductOutbound;

  private final ApplicationContext applicationContext;

  private final BusinessPartnerRepository businessPartnerRepository;

  private final KafkaPublisher kafkaProducer;

  private final ObjectMapper objectMapper;

  private final PreOrderConfig preOrderConfig;

  @Value("${product.history.update.event}")
  private boolean productHistoryUpdateThroughEvent;

  @Override
  public List<UpdatedProductHistory> createAudit(List<UpdatedProductHistory> logList, boolean historySolrUpdateNewEvent) {
    List<UpdatedProductHistory> updatedProductHistories = this.updatedProductHistoryRepository.saveAll(logList);
    if (!historySolrUpdateNewEvent) {
      saveUpdatedProductHistoryToSolr(logList);
    }
    return updatedProductHistories;
  }

  @Override
  public void saveUpdatedProductHistoryToSolr(List<UpdatedProductHistory> logList) {
    if (CollectionUtils.isNotEmpty(logList)) {
      this.solrHistoryCollectionRepository.addDocument(SummaryFilterUtil.toSolrInputDocumentsForHistory(logList));
    }
  }


  @Override
  public Page<UpdatedProductHistory> getAuditLogsForProduct(Pageable pageable,
      String gdnSku) {
    String productSku = productBusinessPartnerRepository.findProductSkuByItemSku(gdnSku);
    return this.updatedProductHistoryRepository
        .findByGdnSkuAndProductSkuAndOnlineStatusTrue(gdnSku, productSku, pageable);
  }

  @Override
  @Async
  public void saveUpdateProductLevel3Audit(String businessPartnerCode, String gdnSku,
      UpdateProductItemLevel3Model savedProductValue, UpdateProductItemLevel3Model updatedProductValue,
      String accessChannel, String productSku, String name, boolean needCorrection, String username) throws Exception {
    if (StringUtils.isNotBlank(username)) {
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
      mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    }
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    Map<UpdateProductActivity, String> propertyMapper = this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class);
    for (Entry<UpdateProductActivity, String> propertyMapperElement : propertyMapper.entrySet()) {
      setAuditLogs(businessPartnerCode, gdnSku, savedProductValue, updatedProductValue,
        accessChannel, productSku, name, propertyMapperElement, auditLogs);
    }
    if (needCorrection) {
      auditLogs.forEach(auditLog -> {
        auditLog.setActivity(auditLog.getActivity() + Constants.NEED_REVISION);
      });
    }

    for (UpdatedProductHistory updatedProductHistory : auditLogs) {
      updatedProductHistory.setOnlineStatus(true);
      if (updatedProductValue.getPickupPointCode() != null) {
        updatedProductHistory.setPickupPointCode(updatedProductValue.getPickupPointCode());
      } else {
        updatedProductHistory.setPickupPointCode(Constants.HYPHEN);
      }
    }
    if (productHistoryUpdateThroughEvent) {
      auditLogs.forEach(this::publishProductUpdateHistoryEvent);
    } else {
      createAudit(auditLogs, false);
    }
  }

  private void setAuditLogs(String businessPartnerCode, String gdnSku,
    UpdateProductItemLevel3Model savedProductValue,
    UpdateProductItemLevel3Model updatedProductValue, String accessChannel, String productSku,
    String name, Entry<UpdateProductActivity, String> propertyMapperElement,
    List<UpdatedProductHistory> auditLogs)
    throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
    UpdateProductActivity actionKey = propertyMapperElement.getKey();
    String propertyName = propertyMapperElement.getValue();
    if (ATTRIBUTE_MAP.equals(propertyMapperElement.getValue())) {
      String[] oldAttributes = BeanUtils.getArrayProperty(savedProductValue, ATTRIBUTE_MAP);
      String[] newAttributes = BeanUtils.getArrayProperty(updatedProductValue, ATTRIBUTE_MAP);
      Map<String, String> oldAttributeValueMap = new HashMap<>();
      Map<String, String> newAttributeValueMap = new HashMap<>();
      getAttributeValueMap(oldAttributes, oldAttributeValueMap);
      getAttributeValueMap(newAttributes, newAttributeValueMap);
      for (String oldAttribute : oldAttributeValueMap.keySet()) {
        String oldValue = oldAttributeValueMap.get(oldAttribute);
        String newValue = newAttributeValueMap.get(oldAttribute);
        getAuditLogs(businessPartnerCode, gdnSku, accessChannel, auditLogs, actionKey, oldValue, newValue,
            oldAttribute, productSku, name, Constants.HYPHEN);
      }
    } else if (WHOLESALE_RULES.equals(propertyMapperElement.getValue()))
      handleWholesaleRulesAudit(businessPartnerCode, gdnSku, savedProductValue, updatedProductValue,
        accessChannel, productSku, name, propertyName, actionKey, auditLogs);
    else if (STOCK_VALUE.equals(propertyMapperElement.getValue()) && ObjectUtils.notEqual(
      Optional.ofNullable(updatedProductValue.getAvailableStockLevel2()),
      Optional.ofNullable(savedProductValue.getAvailableStockLevel2())))
      handleStockValueAudit(businessPartnerCode, gdnSku, savedProductValue, updatedProductValue,
        accessChannel, productSku, name, propertyName, auditLogs);
    else if (SIZE_CHART_CODE.equals(propertyMapperElement.getValue()) && updatedProductValue.isSizeChartChanged()) {
      handleSizeChartValueAudit(businessPartnerCode, gdnSku, savedProductValue, updatedProductValue,
        accessChannel, productSku, name, propertyName, auditLogs);
      
    } else {
      String oldValue = BeanUtils.getProperty(savedProductValue, propertyName);
      String newValue = BeanUtils.getProperty(updatedProductValue, propertyName);
      getAuditLogs(businessPartnerCode, gdnSku, accessChannel, auditLogs, actionKey, oldValue, newValue, null,
        productSku, name, updatedProductValue.getPickupPointCode());
    }
  }

  private void handleSizeChartValueAudit(String businessPartnerCode, String gdnSku,
    UpdateProductItemLevel3Model savedProductValue,
    UpdateProductItemLevel3Model updatedProductValue, String accessChannel, String productSku,
    String name, String propertyName, List<UpdatedProductHistory> auditLogs)
    throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
    UpdateProductActivity actionKey = UpdateProductActivity.SIZE_CHART_CHANGE;
    String oldValue = BeanUtils.getProperty(savedProductValue, propertyName);
    String newValue = BeanUtils.getProperty(updatedProductValue, propertyName);
    getAuditLogs(businessPartnerCode, gdnSku, accessChannel, auditLogs, actionKey,
      Optional.ofNullable(oldValue).orElse(Constants.HYPHEN),
      Optional.ofNullable(newValue).orElse(Constants.HYPHEN), null, productSku, name, null);
  }

  private void handleStockValueAudit(String businessPartnerCode, String gdnSku,
    UpdateProductItemLevel3Model savedProductValue,
    UpdateProductItemLevel3Model updatedProductValue, String accessChannel, String productSku,
    String name, String propertyName, List<UpdatedProductHistory> auditLogs)
    throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
    UpdateProductActivity actionKey;
    String oldValue = BeanUtils.getProperty(savedProductValue, propertyName);
    String newValue = BeanUtils.getProperty(updatedProductValue, propertyName);
    if (Objects.nonNull(updatedProductValue.getAvailableStockLevel2()) ) {
      newValue = String.valueOf(updatedProductValue.getAvailableStockLevel2());
      Integer oldStock =
        Optional.of(savedProductValue).map(UpdateProductItemLevel3Model::getAvailableStockLevel2)
        .orElse(null);
      oldValue = Objects.nonNull(oldStock) ? Integer.toString(oldStock) : StringUtils.EMPTY;
    }
    actionKey = UpdateProductActivity.STOCK_VALUE;
    getAuditLogs(businessPartnerCode, gdnSku, accessChannel, auditLogs, actionKey, oldValue, newValue, null,
      productSku, name, updatedProductValue.getPickupPointCode());
  }

  private void handleWholesaleRulesAudit(String businessPartnerCode, String gdnSku,
    UpdateProductItemLevel3Model savedProductValue,
    UpdateProductItemLevel3Model updatedProductValue, String accessChannel, String productSku,
    String name, String propertyName, UpdateProductActivity actionKey,
    List<UpdatedProductHistory> auditLogs)
    throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
    String oldValue = BeanUtils.getProperty(savedProductValue, propertyName);
    if (StringUtils.isEmpty(savedProductValue.getWholesaleRules()) && StringUtils
        .isNotEmpty(updatedProductValue.getWholesaleRules())) {
      actionKey = UpdateProductActivity.ADD_WHOLE_SALE_RULES;
      oldValue = StringUtils.EMPTY;
    }
    String newValue = BeanUtils.getProperty(updatedProductValue, propertyName);
    getAuditLogs(businessPartnerCode, gdnSku, accessChannel, auditLogs, actionKey, oldValue, newValue, null,
      productSku, name, updatedProductValue.getPickupPointCode());
  }

  @Override
  public List<String> getMerchantModifiedFields(UpdateProductItemLevel3Model savedProductValue,
      UpdateProductItemLevel3Model updatedProductValue) throws Exception {
    List<String> modifiedFields = new ArrayList<>();
    Map<UpdateProductActivity, String> propertyMapper = this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class);
    for (Entry<UpdateProductActivity, String> propertyMapperElement : propertyMapper.entrySet()) {
      UpdateProductActivity actionKey = propertyMapperElement.getKey();
      String propertyName = propertyMapperElement.getValue();
      if (ATTRIBUTE_MAP.equals(propertyMapperElement.getValue())) {
        String[] oldAttributes = BeanUtils.getArrayProperty(savedProductValue, ATTRIBUTE_MAP);
        String[] newAttributes = BeanUtils.getArrayProperty(updatedProductValue, ATTRIBUTE_MAP);
        Map<String, String> oldAttributeValueMap = new HashMap<>();
        Map<String, String> newAttributeValueMap = new HashMap<>();
        getAttributeValueMap(oldAttributes, oldAttributeValueMap);
        getAttributeValueMap(newAttributes, newAttributeValueMap);
        for (String oldAttribute : oldAttributeValueMap.keySet()) {
          String oldValue = oldAttributeValueMap.get(oldAttribute);
          String newValue = newAttributeValueMap.get(oldAttribute);
          addToModifiedFields(modifiedFields, newValue, oldValue, oldAttribute, actionKey);
        }
      } else {
        String oldValue = BeanUtils.getProperty(savedProductValue, propertyName);
        String newValue = BeanUtils.getProperty(updatedProductValue, propertyName);
        addToModifiedFields(modifiedFields, newValue, oldValue, propertyName, actionKey);
      }
    }
    return modifiedFields;
  }

  private void addToModifiedFields(List<String> modifiedFields, String newValue, String oldValue,
      String fieldName, UpdateProductActivity actionKey) {
    if (UpdateProductActivity.USP.getDesc().equalsIgnoreCase(actionKey.getDesc())) {
      fieldName = Constants.USP_EDITED;
    }
    if (UpdateProductActivity.URL_VIDEO.getDesc().equalsIgnoreCase(actionKey.getDesc())) {
      fieldName = Constants.URL_VIDEO_EDITED;
    }
    if(!StringUtils.equals(newValue, oldValue)){
      if (UpdateProductActivity.USP.getDesc().equalsIgnoreCase(actionKey.getDesc())
          || UpdateProductActivity.PRODUCT_DESC.getDesc().equalsIgnoreCase(actionKey.getDesc())
          || UpdateProductActivity.URL_VIDEO.getDesc().equalsIgnoreCase(actionKey.getDesc())) {
        if (!StringUtils.equals(formattedHTMLText(oldValue), formattedHTMLText(newValue))) {
          modifiedFields.add(fieldName);
        }
      } else if (StringUtils.isNotEmpty(newValue)) {
        modifiedFields.add(fieldName);
      } else if (StringUtils.isNotEmpty(oldValue) && (
          UpdateProductActivity.MERCHANT_SKU.getDesc().equals(actionKey.getDesc()) || UpdateProductActivity.ATTRIBUTES
              .getDesc().equals(actionKey.getDesc()))) {
        modifiedFields.add(fieldName);
      }
    }
  }

  @Override
  public void addAuditLogsForProductHistoryUpdate(AuditTrailDto auditTrailRequest,
      List<UpdatedProductHistory> auditLogs, String accessChannel) throws Exception {
    UpdatedProductHistory updatedProductHistory = getAuditLogs(auditTrailRequest.getBusinessPartnerCode(),
        auditTrailRequest.getGdnSku(), accessChannel, auditLogs, Enum.valueOf(UpdateProductActivity.class,
        auditTrailRequest.getActionKey()), auditTrailRequest.getOldValue(), auditTrailRequest.getNewValue(),
        auditTrailRequest.getAttributeName(), auditTrailRequest.getProductSku(), auditTrailRequest.getName(), auditTrailRequest.getPickupPointCode());
    if (Objects.nonNull(updatedProductHistory)) {
      updatedProductHistory.setPickupPointCode(auditTrailRequest.getPickupPointCode());
      updatedProductHistory.setOnlineStatus(auditTrailRequest.isOnlineStatus());
    }
  }

  @Override
  public Page<HistoryResponse> findProductHistoryByProductSkuAndKeyword(HistoryRequest historyRequest, int page,
      int size) {
    Page<UpdatedProductHistory> logAuditTrailUpdatedProducts = null;
    if (StringUtils.isNotBlank(historyRequest.getKeyword())) {
      if (Constants.ACTIVITY.equalsIgnoreCase(historyRequest.getSearchField())) {
        logAuditTrailUpdatedProducts = updatedProductHistoryRepository
            .findByProductSkuAndAccessTimeBetweenAndActivityAndOnlineStatusTrue(historyRequest.getProductSku(),
                historyRequest.getStartDate(), historyRequest.getEndDate(), historyRequest.getKeyword(),
                PageRequest.of(page, size));
      } else {
        logAuditTrailUpdatedProducts = updatedProductHistoryRepository
            .findByProductSkuAndAccessTimeBetweenAndVariantNameAndOnlineStatusTrue(historyRequest.getProductSku(),
                historyRequest.getStartDate(), historyRequest.getEndDate(), historyRequest.getKeyword(),
                PageRequest.of(page, size));
      }
    } else {
      logAuditTrailUpdatedProducts = updatedProductHistoryRepository
          .findByProductSkuAndAccessTimeBetweenAndOnlineStatusTrueOrderByAccessTimeDesc(historyRequest.getProductSku(),
              historyRequest.getStartDate(), historyRequest.getEndDate(), PageRequest.of(page, size));
    }
    List<HistoryResponse> historyResponseList =
        ConverterUtil.toHistoryResponseFromLogAuditTrailUpdateProduct(logAuditTrailUpdatedProducts.getContent());
    return new PageImpl<>(historyResponseList, PageRequest.of(page, size),
        logAuditTrailUpdatedProducts.getTotalElements());
  }

  @Override
  public void addAuditLogsForVatUpdate(List<ProductItemBusinessPartner> productItemBusinessPartnerList, String itemName,
      String oldValue, String newValue) throws JsonProcessingException {
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerList) {
      ProductBusinessPartner productBusinessPartner = productItemBusinessPartner.getProductBusinessPartner();
      if (Objects.nonNull(productBusinessPartner)) {
        getAuditLogs(productBusinessPartner.getBusinessPartnerId(), productItemBusinessPartner.getGdnProductItemSku(),
            GdnMandatoryRequestParameterUtil.getChannelId(), auditLogs, UpdateProductActivity.APPLICABLE_FOR_VAT,
            oldValue, newValue, null, productBusinessPartner.getGdnProductSku(), itemName, Constants.HYPHEN);
      }
    }
    if (CollectionUtils.isNotEmpty(auditLogs)) {
      GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = this.xProductOutbound
          .getItemPickupPointCodeByItemSkus(ConverterUtil.toSimpleListStringRequest(auditLogs));
      Map<String, String> itemSkuAndPickupPointCodeMap =
          ConverterUtil.toItemSkuAndPickupPointCodeMap(itemPickupPointCodeByItemSkus.getContent());
      auditLogs =
          CommonUtils.setOnlineStatusAndPickupPointCode(auditLogs, itemSkuAndPickupPointCodeMap);
      if (productHistoryUpdateThroughEvent) {
        auditLogs.forEach(this::publishProductUpdateHistoryEvent);
      } else {
        createAudit(auditLogs, false);
      }
    }
  }

  private UpdatedProductHistory getAuditLogs(String businessPartnerCode, String gdnSku, String accessChannel,
      List<UpdatedProductHistory> auditLogs, UpdateProductActivity actionKey, String oldValue, String newValue,
      String attributeName, String productSku, String name, String pickupPointCode) {
    UpdatedProductHistory auditLog =
        createAuditLogsForItemUpdate(businessPartnerCode, gdnSku, actionKey, oldValue, newValue);
    if (Objects.nonNull(auditLog)) {
      auditLog.setPickupPointCode(pickupPointCode);
      auditLog.setActivity(StringUtils.isNotEmpty(attributeName) ?
          actionKey.getDesc() + StringUtils.SPACE + attributeName :
          actionKey.getDesc());
      if (StringUtils.isNotBlank(accessChannel)) {
        auditLog.setAccessChannel(accessChannel);
      }
      auditLog.setProductSku(productSku);
      auditLog.setGdnName(name);
      auditLogs.add(auditLog);
    }
    return auditLog;
  }

  private UpdatedProductHistory createAuditLogsForItemUpdate(String businessPartnerCode, String gdnSku,
    UpdateProductActivity actionKey, String oldValue, String newValue) {

    if (StringUtils.equals(newValue, oldValue)) {
      return null;
    }

    LOGGER.debug("Update product level 3 with audit Action={}, MerchantCode={}, OldValue={}, NewValue={} ",
      actionKey.getDesc(), businessPartnerCode, oldValue, newValue);

    if (isHtmlFormattedTextChange(actionKey, oldValue, newValue)) {
      return createLogAuditTrailUpdatedProduct(businessPartnerCode, gdnSku, actionKey.getDesc(), oldValue, newValue);
    }

    if (isDeletion(actionKey, oldValue, newValue)) {
      return createLogAuditTrailUpdatedProduct(businessPartnerCode, gdnSku, actionKey.getDesc(), oldValue, Constants.DELIMITER_DASH);
    }

    if (isStockOrSizeChartChange(actionKey, newValue)) {
      return createLogAuditTrailUpdatedProduct(businessPartnerCode, gdnSku, actionKey.getDesc(), oldValue, newValue);
    }

    if (StringUtils.isNotEmpty(newValue)) {
      return createLogAuditTrailUpdatedProduct(businessPartnerCode, gdnSku, actionKey.getDesc(), oldValue, newValue);
    }

    return null;
  }

  private boolean isHtmlFormattedTextChange(UpdateProductActivity actionKey, String oldValue, String newValue) {
    return (UpdateProductActivity.USP.getDesc().equalsIgnoreCase(actionKey.getDesc())
      || UpdateProductActivity.PRODUCT_DESC.getDesc().equalsIgnoreCase(actionKey.getDesc())
      || UpdateProductActivity.URL_VIDEO.getDesc().equalsIgnoreCase(actionKey.getDesc()))
      && !StringUtils.equals(formattedHTMLText(oldValue), formattedHTMLText(newValue));
  }

  private boolean isDeletion(UpdateProductActivity actionKey, String oldValue, String newValue) {
    return StringUtils.isNotEmpty(oldValue) && StringUtils.isEmpty(newValue)
      && (UpdateProductActivity.MERCHANT_SKU.getDesc().equals(actionKey.getDesc())
      || UpdateProductActivity.ATTRIBUTES.getDesc().equals(actionKey.getDesc())
      || UpdateProductActivity.PICKUP_POINT_DELETED.getDesc().equals(actionKey.getDesc()));
  }

  private boolean isStockOrSizeChartChange(UpdateProductActivity actionKey, String newValue) {
    return StringUtils.isNotEmpty(newValue)
      && (UpdateProductActivity.STOCK_VALUE.getDesc().equals(actionKey.getDesc())
      || UpdateProductActivity.SIZE_CHART_CHANGE.getDesc().equals(actionKey.getDesc()));
  }


  private static String formattedHTMLText(String request) {
    if (StringUtils.isNotBlank(request)) {
      if (request.contains("&lt;") || request.contains("&gt;") || request.contains("&amp;")) {
        request = Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), customWhitelist());
        return StringEscapeUtils.unescapeHtml3(request);
      } else {
        return StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, customWhitelist()));
      }
    }
    return request;
  }

  private static Whitelist customWhitelist() {
    return Whitelist.relaxed().addTags(IFRAME)
        .addAttributes(IFRAME, "align", "alt", "height", "src", "title", "width", "allowfullscreen");
  }

  private void getAttributeValueMap(String[] oldAttributes, Map<String, String> oldAttributeValueMap) {
    if (Objects.nonNull(oldAttributes)) {
      for (String attribute : oldAttributes) {
        String[] value = attribute.split(Constants.DELIMITER);
        if (value.length > 1) {
          oldAttributeValueMap.put(value[0], value[1]);
        } else {
          oldAttributeValueMap.put(value[0], StringUtils.EMPTY);
        }
      }
    }
  }

  @Override
  public void saveUpdateProductLevel3AuditWithDescription(String businessPartnerCode, String gdnSku, String actualItemSku,
        UpdateProductItemLevel3Model savedProductValue, UpdateProductItemLevel3Model updatedProductValue, String
        accessChannel) throws Exception {
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    Map<UpdateProductActivity, String> propertyMapper = this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class);
    for (Entry<UpdateProductActivity, String> propertyMapperElement : propertyMapper.entrySet()) {
      UpdateProductActivity actionKey = propertyMapperElement.getKey();
      String propertyName = propertyMapperElement.getValue();
      String oldValue = BeanUtils.getProperty(savedProductValue, propertyName);
      String newValue = BeanUtils.getProperty(updatedProductValue, propertyName);
      UpdatedProductHistory auditLog =
          createAuditLogsForItemUpdate(businessPartnerCode, gdnSku, actionKey, oldValue, newValue);
      if (Objects.nonNull(auditLog)) {
        auditLog.setPickupPointCode(updatedProductValue.getPickupPointCode());
        if (StringUtils.isNotBlank(accessChannel)) {
          auditLog.setAccessChannel(accessChannel);
        }
        if (StringUtils.isNotBlank(actualItemSku)) {
          auditLog.setDescription("Due to sync/unsync of " + actualItemSku + " itemSku");
        }
        auditLogs.add(auditLog);
      }
    }

    for (UpdatedProductHistory updatedProductHistory : auditLogs) {
      updatedProductHistory.setOnlineStatus(true);
      if (updatedProductValue.getPickupPointCode() != null) {
        updatedProductHistory.setPickupPointCode(updatedProductValue.getPickupPointCode());
      } else {
        updatedProductHistory.setPickupPointCode(Constants.HYPHEN);
      }
    }
    if (productHistoryUpdateThroughEvent) {
      auditLogs.forEach(this::publishProductUpdateHistoryEvent);
    } else {
      createAudit(auditLogs, false);
    }
  }

  @Override
  public void saveUpdateProductLevel3AuditForWholeSale(String businessPartnerCode, String gdnSku,
      String activity, String oldValue, String newValue, String productSku, String name) throws Exception {
    UpdatedProductHistory auditLog = createLogAuditTrailUpdatedProduct(businessPartnerCode,
        gdnSku, activity, oldValue, newValue);
    auditLog.setProductSku(productSku);
    auditLog.setGdnName(name);
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    updatedProductHistoryList.add(auditLog);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = this.xProductOutbound
        .getItemPickupPointCodeByItemSkus(ConverterUtil.toSimpleListStringRequest(updatedProductHistoryList));
    Map<String, String> itemSkuAndPickupPointCodeMap =
        ConverterUtil.toItemSkuAndPickupPointCodeMap(itemPickupPointCodeByItemSkus.getContent());
    updatedProductHistoryList =
        CommonUtils.setOnlineStatusAndPickupPointCode(updatedProductHistoryList, itemSkuAndPickupPointCodeMap);
    auditLog = updatedProductHistoryList.get(0);
    if (productHistoryUpdateThroughEvent) {
      publishProductUpdateHistoryEvent(auditLog);
    } else {
      createAudit(Arrays.asList(auditLog), false);
    }
  }

  @Override
  public void saveUpdateProductLevel3AuditForMigration(String businessPartnerCode, String gdnSku)
      throws JsonProcessingException {
    UpdatedProductHistory auditLog = createLogAuditTrailUpdatedProduct(businessPartnerCode, gdnSku);
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    updatedProductHistoryList.add(auditLog);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = this.xProductOutbound
        .getItemPickupPointCodeByItemSkus(ConverterUtil.toSimpleListStringRequest(updatedProductHistoryList));
    Map<String, String> itemSkuAndPickupPointCodeMap =
        ConverterUtil.toItemSkuAndPickupPointCodeMap(itemPickupPointCodeByItemSkus.getContent());
    updatedProductHistoryList =
        CommonUtils.setOnlineStatusAndPickupPointCode(updatedProductHistoryList, itemSkuAndPickupPointCodeMap);
    auditLog = updatedProductHistoryList.get(0);
    if (productHistoryUpdateThroughEvent) {
      publishProductUpdateHistoryEvent(auditLog);
    } else {
      createAudit(Arrays.asList(auditLog), false);
    }
  }

  private void publishProductUpdateHistoryEvent(UpdatedProductHistory auditLog) {
    AuditTrailListRequest auditTrailListRequest = CommonUtils
        .getAuditTrailRequestForL3History(auditLog.getBusinessPartnerCode(), auditLog.getProductSku(),
            auditLog.getGdnName(), auditLog.getActivity(), auditLog.getActivity(), auditLog.getOldValues(),
            auditLog.getNewValues(), auditLog.getChangedBy(), auditLog.getRequestId(), auditLog.getClientHost(),
            auditLog.getGdnSku(), auditLog.getPickupPointCode());
    auditTrailListRequest.setUpdateDirectlyToDB(true);
    log.info("Publishing the event {} with message {} for updating history of productSku {} ",
        PRODUCT_SKU_UPDATE_HISTORY, auditTrailListRequest, auditLog.getProductSku());
    kafkaProducer.send(PRODUCT_SKU_UPDATE_HISTORY, auditLog.getProductSku(), auditTrailListRequest);
  }

  private UpdatedProductHistory createLogAuditTrailUpdatedProduct(String businessPartnerCode, String gdnSku) {
    UpdatedProductHistory auditLog = new UpdatedProductHistory();
    auditLog.setRequestId(Constants.MIGRATION.toLowerCase());
    auditLog.setChangedBy(Constants.MIGRATION.toLowerCase());
    auditLog.setClientHost(Constants.MIGRATION.toLowerCase());
    auditLog.setAccessTime(Calendar.getInstance().getTime());
    auditLog.setActivity(UpdateProductActivity.SYNC_CONTENT.getDesc());
    auditLog.setBusinessPartnerCode(businessPartnerCode);
    auditLog.setGdnSku(gdnSku);
    auditLog.setOldValues("false");
    auditLog.setNewValues("true");
    auditLog.setStatus(Boolean.TRUE);
    return auditLog;
  }

  private UpdatedProductHistory createLogAuditTrailUpdatedProduct(String businessPartnerCode,
      String gdnSku, String activity, String oldValue, String newValue) {
    UpdatedProductHistory auditLog = new UpdatedProductHistory();
    String requestId = mandatoryParameterHelper.getRequestId();
    String changedBy = mandatoryParameterHelper.getUsername();
    String clientHost = mandatoryParameterHelper.getClientId();
    auditLog.setRequestId(Objects.nonNull(requestId) ? requestId : UUID.randomUUID().toString());
    auditLog.setChangedBy(Objects.nonNull(changedBy) ? changedBy : DEFAULT_USER);
    auditLog.setClientHost(Objects.nonNull(clientHost) ? clientHost : CLIENT_HOST_KEY);
    auditLog.setAccessTime(Calendar.getInstance().getTime());
    auditLog.setActivity(activity);
    auditLog.setBusinessPartnerCode(businessPartnerCode);
    auditLog.setGdnSku(gdnSku);
    if (UpdateProductActivity.NORMAL_PRICE.getDesc().equalsIgnoreCase(activity) || UpdateProductActivity.SELLING_PRICE
        .getDesc().equalsIgnoreCase(activity) || UpdateProductActivity.STOCK_VALUE.getDesc().equalsIgnoreCase(activity)) {
      NumberFormat numberFormat = new DecimalFormat(FORMAT);
      oldValue = StringUtils.isNotEmpty(oldValue) ?
        numberFormat.format(Double.parseDouble(oldValue)) :
        numberFormat.format(0.0);
      newValue = numberFormat.format(Double.parseDouble(newValue));
    }
    auditLog.setOldValues(StringUtils.abbreviate(oldValue, MAX_CHARACTER_FOR_AUDIT_VALUE));
    auditLog.setNewValues(StringUtils.abbreviate(newValue, MAX_CHARACTER_FOR_AUDIT_VALUE));
    auditLog.setStatus(Boolean.TRUE);
    return auditLog;
  }

  @Override
  public void saveCreateProductLevel3Audit(String businessPartnerCode,
      ProductAndItemActivationRequest productLevel3Request, ProductBusinessPartner productBusinessPartner) throws Exception {
    final Calendar now = Calendar.getInstance();
    final String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    final String username = GdnMandatoryRequestParameterUtil.getUsername();
    ProfileResponse profileResponse = businessPartnerRepository
        .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    List<String> salesChannel = CommonUtils.salesChannelFromProfileResponse(profileResponse);
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    for (ItemActivationRequest itemRequest : productLevel3Request.getItems()) {
      String createdBy = this.productBusinessPartnerRepository.getCreatedByItemBusinessPartnerByGdnProductItemSku(
          itemRequest.getItemSku());
      if (StringUtils.isEmpty(createdBy)) {
        createdBy = username;
      }
      for (ItemPickupPointActivationRequest itemPickupPointActivationRequest : itemRequest.getItemPickupPoints()) {
        double normalPrice = itemPickupPointActivationRequest.getPrice().iterator().next().getListPrice();
        double discountPrice = itemPickupPointActivationRequest.getPrice().iterator().next().getOfferPrice();
        Integer stock = null;
        Integer preOrderQuota = null;
        for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner.getProductItemBusinessPartners()) {
          if (productItemBusinessPartner.getGdnProductItemSku().equals(itemRequest.getItemSku())) {
            stock = productItemBusinessPartner.getStock();
            preOrderQuota = productItemBusinessPartner.getPreOrderQuota();
          }
        }

        auditLogs.add(
            generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), PRODUCT_MERCHANT_ACTIVE, now.getTime(),
                businessPartnerCode, requestId, Constants.SYSTEM, now.getTime(), null, null,
                productBusinessPartner.getGdnProductSku(), productBusinessPartner.getProductName(),
                itemPickupPointActivationRequest.getPickupPointCode()));

        initialItemViewConfigDetails(itemRequest, itemPickupPointActivationRequest, auditLogs, productBusinessPartner,
            now, requestId, createdBy, salesChannel);
        initialB2bDetails(itemRequest, itemPickupPointActivationRequest, auditLogs, productBusinessPartner,
            now, requestId, createdBy, salesChannel);

        auditLogs.add(generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), INITIAL_VALUE_OF_STOCK_QUANTITY, now.getTime(),
            businessPartnerCode, requestId, createdBy, null, DASH_DELIMITER, String.valueOf(stock),
            productBusinessPartner.getGdnProductSku(), productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));

        if (preOrderConfig.isPoQuotaFeatureSwitch() && CommonUtils.getBusinessPartnerFlagValue(
            profileResponse, Constants.BLIBLI_OMG) && Optional.ofNullable(
            productBusinessPartner.isPreOrder()).orElse(false) && Objects.nonNull(preOrderQuota)) {
          auditLogs.add(generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(),
              INITIAL_VALUE_OF_PREORDER_QUOTA, now.getTime(), businessPartnerCode, requestId,
              createdBy, null, DASH_DELIMITER, String.valueOf(preOrderQuota),
              productBusinessPartner.getGdnProductSku(), productBusinessPartner.getProductName(),
              itemPickupPointActivationRequest.getPickupPointCode()));
        }

        if (discountPrice < normalPrice) {
          auditLogs.add(generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), INITIAL_VALUE_OF_DISCOUNT_PRICE,
              now.getTime(), businessPartnerCode, requestId, createdBy, null, DASH_DELIMITER, String.valueOf(discountPrice), productBusinessPartner.getGdnProductSku(),
              productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));
        }

        auditLogs.add(
            generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), INITIAL_VALUE_OF_NORMAL_PRICE, now.getTime(),
                businessPartnerCode, requestId, createdBy, null, DASH_DELIMITER, String.valueOf(normalPrice),
                productBusinessPartner.getGdnProductSku(), productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));
      }
    }
    initialSalesChannelHistory(auditLogs, productBusinessPartner, now, requestId, salesChannel);
    if (productHistoryUpdateThroughEvent) {
      auditLogs.forEach(this::publishProductUpdateHistoryEvent);
    } else {
      createAudit(auditLogs, false);
    }
  }

  public void initialItemViewConfigDetails(ItemActivationRequest itemRequest, ItemPickupPointActivationRequest itemPickupPointActivationRequest,
      List<UpdatedProductHistory> auditLogs, ProductBusinessPartner productBusinessPartner, Calendar now,
      String requestId, String createdBy, List<String> salesChannel) {

    ItemViewConfigRequest itemViewConfig = itemPickupPointActivationRequest.getItemViewConfigs().stream()
        .filter(itemViewConfigRequest -> Constants.DEFAULT.equals(itemViewConfigRequest.getChannel())).findFirst().orElse(null);
    if (salesChannel.contains(Constants.B2C_SELLER_CHANNEL) && Objects.nonNull(itemViewConfig)) {
      boolean buyable = itemViewConfig.isBuyable();
      boolean discoverable = itemViewConfig.isDiscoverable();
      auditLogs.add(generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), INITIAL_VALUE_OF_DISPLAYABLE_FLAG,
          now.getTime(), productBusinessPartner.getBusinessPartnerId(), requestId, createdBy, null, DASH_DELIMITER,
          String.valueOf(discoverable), productBusinessPartner.getGdnProductSku(),
          productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));

      auditLogs.add(generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), INITIAL_VALUE_OF_BUYABLE_FLAG,
          now.getTime(), productBusinessPartner.getBusinessPartnerId(), requestId, createdBy, null, DASH_DELIMITER,
          String.valueOf(buyable), productBusinessPartner.getGdnProductSku(),
          productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));
    }

    ItemViewConfigRequest b2bItemViewConfig = itemPickupPointActivationRequest.getItemViewConfigs().stream()
        .filter(itemViewConfigRequest -> Constants.B2B_CHANNEL.equals(itemViewConfigRequest.getChannel())).findFirst().orElse(null);
    if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL) && Objects.nonNull(b2bItemViewConfig)) {
      boolean b2bBuyable = b2bItemViewConfig.isBuyable();
      boolean b2bDiscoverable = b2bItemViewConfig.isDiscoverable();
      auditLogs.add(generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), UpdateProductActivity.INITIAL_B2B_BUYABLE.name(),
          now.getTime(), productBusinessPartner.getBusinessPartnerId(), requestId, createdBy, null, DASH_DELIMITER,
          String.valueOf(b2bBuyable), productBusinessPartner.getGdnProductSku(),
          productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));

      auditLogs.add(generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), UpdateProductActivity.INITIAL_B2B_DISCOVERABLE.name(),
          now.getTime(), productBusinessPartner.getBusinessPartnerId(), requestId, createdBy, null, DASH_DELIMITER,
          String.valueOf(b2bDiscoverable), productBusinessPartner.getGdnProductSku(),
          productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));
    }

  }

  public void initialB2bDetails(ItemActivationRequest itemRequest, ItemPickupPointActivationRequest itemPickupPointActivationRequest,
      List<UpdatedProductHistory> auditLogs, ProductBusinessPartner productBusinessPartner, Calendar now,
      String requestId, String createdBy, List<String> salesChannel) {
    if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL) && Objects.nonNull(itemPickupPointActivationRequest.getB2bFields())) {
      Double basePrice = itemPickupPointActivationRequest.getB2bFields().getBasePrice();
      boolean managed = itemPickupPointActivationRequest.getB2bFields().isManaged();
        auditLogs.add(
            generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), UpdateProductActivity.INITIAL_B2B_BASE_PRICE.name(),
                now.getTime(), productBusinessPartner.getBusinessPartnerId(), requestId, createdBy, null, DASH_DELIMITER,
                Objects.nonNull(basePrice) ? String.valueOf(basePrice) : Constants.HYPHEN, productBusinessPartner.getGdnProductSku(),
                productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));
      auditLogs.add(
          generateLogAuditTrailUpdatedProduct(itemRequest.getItemSku(), UpdateProductActivity.INITIAL_B2B_MANAGED.name(),
              now.getTime(), productBusinessPartner.getBusinessPartnerId(), requestId, createdBy, null, DASH_DELIMITER,
              String.valueOf(managed) , productBusinessPartner.getGdnProductSku(),
              productBusinessPartner.getProductName(), itemPickupPointActivationRequest.getPickupPointCode()));
    }
  }

  public void initialSalesChannelHistory(List<UpdatedProductHistory> auditLogs,
      ProductBusinessPartner productBusinessPartner, Calendar now, String requestId, List<String> salesChannel) {
    if (salesChannel.size() > 1) {
      String sellerChannel = Constants.HYPHEN;
      if (productBusinessPartner.isB2bActivated() && productBusinessPartner.isB2cActivated()) {
        sellerChannel = Constants.B2C_CHANNEL + Constants.COMMA + Constants.B2B_CHANNEL;
      } else if (productBusinessPartner.isB2bActivated()) {
        sellerChannel = Constants.B2B_CHANNEL;
      } else if (productBusinessPartner.isB2cActivated()) {
        sellerChannel = Constants.B2C_CHANNEL;
      }
      auditLogs.add(generateLogAuditTrailUpdatedProduct(Constants.DEFAULT,
          UpdateProductActivity.INITIAL_B2B_SALES_CHANNEL.name(), now.getTime(),
          productBusinessPartner.getBusinessPartnerId(), requestId,
          productBusinessPartner.getCreatedBy(), null, DASH_DELIMITER, sellerChannel,
          productBusinessPartner.getGdnProductSku(), productBusinessPartner.getProductName(),
          Constants.HYPHEN));
    }
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void createAuditTrailForAllItemSkus(String businessPartnerCode, List<String> skus, String sku, ProductAndItemsResponse
          savedProductData, ProductAndItemsResponse updatedProductData, String accessChannel) throws Exception {
    UpdateProductItemLevel3Model savedProductValue =
            updateProductItemLevel3ModelConverter.convertFromProductAndItemsResponse(savedProductData);
    UpdateProductItemLevel3Model updatedProductValue =
            updateProductItemLevel3ModelConverter
                    .convertFromProductAndItemsResponse(updatedProductData);
    skus.forEach(itemSku -> {
      try {
        this.saveUpdateProductLevel3AuditWithDescription(businessPartnerCode, itemSku, sku, savedProductValue, updatedProductValue,
                accessChannel);

      } catch (Exception e) {
        LOGGER.error("Exception caught while creating productLevel3Audit, sku : {}", itemSku, e);
      }
    });
  }

  private UpdatedProductHistory generateLogAuditTrailUpdatedProduct(String gdnSku,
      String activity, Date accessTime, String businessPartnerCode, String requestId,
      String username, Date activatedDate, String oldValues, String newValues, String productSku, String productName, String pickupPointcode) {
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setGdnSku(gdnSku);
    updatedProductHistory.setActivity(activity);
    updatedProductHistory.setAccessTime(accessTime);
    updatedProductHistory.setBusinessPartnerCode(businessPartnerCode);
    updatedProductHistory.setRequestId(requestId);
    updatedProductHistory.setChangedBy(username);
    updatedProductHistory.setActivatedDate(activatedDate);
    updatedProductHistory.setOldValues(oldValues);
    updatedProductHistory.setNewValues(newValues);
    updatedProductHistory.setGdnName(productName);
    updatedProductHistory.setProductSku(productSku);
    updatedProductHistory.setPickupPointCode(pickupPointcode);
    return updatedProductHistory;
  }

  @Override
  public SimpleBooleanResponse isPriceChangedForSku(String sku, Date fromDate) {
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(sku), ErrorMessages.SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(fromDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    Long count = updatedProductHistoryRepository.countByGdnSkuAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(
        sku, fromDate, Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(),
            UpdateProductActivity.SELLING_PRICE.getDesc()));
    return new SimpleBooleanResponse(count > 0);
  }

  @Override
  public SimpleBooleanResponse isPriceChangedForSkuAndPPCode(String sku, Date fromDate, String ppCode) {
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(sku), ErrorMessages.SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(fromDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(ppCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_NULL);
    UpdatedProductHistory updatedProductHistory =
        updatedProductHistoryRepository.findFirstByGdnSkuAndPickupPointCodeAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(
            sku, ppCode, fromDate,
            Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(), UpdateProductActivity.SELLING_PRICE.getDesc()));
    return new SimpleBooleanResponse(Objects.nonNull(updatedProductHistory));
  }

  @Override
  public Page<HistoryResponse> getProductEditHistory(String storeId, String productSku, String keyword, int page,
      int size) {
    Page<UpdatedProductHistory> logAuditTrailUpdatedProducts;
    if (StringUtils.isEmpty(keyword)) {
      logAuditTrailUpdatedProducts = updatedProductHistoryRepository
          .findByProductSkuOrderByAccessTimeDesc(productSku, PageRequest.of(page, size));
    } else {
      logAuditTrailUpdatedProducts = updatedProductHistoryRepository
          .findByProductSkuAndKeyword(productSku, keyword, PageRequest.of(page, size));
    }
    List<HistoryResponse> historyResponseList =
        ConverterUtil.toHistoryResponseFromLogAuditTrailUpdateProduct(logAuditTrailUpdatedProducts.getContent());
    return new PageImpl<>(historyResponseList, PageRequest.of(page, size),
        logAuditTrailUpdatedProducts.getTotalElements());
  }

  @Override
  public Page<HistoryResponse> getProductEditHistoryByAuditTrailId(List<String> auditTrailId, int page, int size,
      long total) {
    List<UpdatedProductHistory> updatedProductHistories =
        updatedProductHistoryRepository.findByAuditTrailIdInOrderByAccessTimeDesc(auditTrailId);
    List<HistoryResponse> historyResponseList =
        ConverterUtil.toHistoryResponseFromLogAuditTrailUpdateProduct(updatedProductHistories);
    return new PageImpl<>(historyResponseList, PageRequest.of(page, size), total);
  }

  @Override
  @Async
  public void deleteFromDb(String storeId) {
    int days = Integer.parseInt(productSystemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE).getValue());
    int batchSize = Integer.parseInt(productSystemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE).getValue());
    int pagesSize = 0;
    if (days != Constants.ZERO) {
      long startTime = Calendar.getInstance().getTimeInMillis();
      Date accessTime = DateUtils.addDays(new Date(), -days);
      LOGGER.info("Deleting entries older than updates date from db: {}", accessTime);
      int page = 0;
      Pageable pageable = PageRequest.of(page, batchSize);
      List<String> auditTrailId;
      do {
        pagesSize = Integer.parseInt(productSystemParameterService
            .findByStoreIdAndVariable(storeId, SystemParameterConstants.UPDATE_HISTORY_DELETE_PAGES_SIZE).getValue());
        auditTrailId = updatedProductHistoryRepository.findAuditTrailIdByAccessTime(accessTime, pageable);
        getUpdatedProductHistoryServiceBean().deleteUpdatedProductHistoryByAuditTrialIds(auditTrailId);
        page = page + 1;
      } while (CollectionUtils.isNotEmpty(auditTrailId) && auditTrailId.size() == batchSize && pagesSize > page);
      long endTime = Calendar.getInstance().getTimeInMillis();
      LOGGER.info("Total time taken for deleting entries from db in milliseconds {} ", endTime - startTime);
    }
  }

  @Override
  @Transactional(timeout = 1200, readOnly = false, rollbackFor = Exception.class)
  public void deleteUpdatedProductHistoryByAuditTrialIds(List<String> auditTrailIds) {
    try {
      auditTrailUpdateProductBackupRepository.saveBackupByAuditTrailIds(auditTrailIds);
      updatedProductHistoryRepository.deleteByAuditTrailIds(auditTrailIds);
    } catch (Exception ex) {
      LOGGER.error("Error while deleting from product history for id : {} ", auditTrailIds, ex);
    }
  }

  @Override
  @Async
  public void deleteFromSolr(String storeId) {
    int days = Integer.valueOf(productSystemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE_SOLR).getValue());
    if (days != Constants.ZERO) {
      long startTime = Calendar.getInstance().getTimeInMillis();
      Date accessTime = DateUtils.addDays(new Date(), -days);
      LOGGER.info("Deleting entries older than updates date from solr: {}", accessTime);
      try {
        solrHistoryCollectionRepository.deleteHistoryFromSolr(days, accessTime);
      } catch (Exception ex) {
        LOGGER.error("Error while deleting from audit trail solr : ", ex);
        return;
      }
      long endTime = Calendar.getInstance().getTimeInMillis();
      LOGGER.info("Total time taken for deleting entries from solr in milliseconds {} ", endTime - startTime);
    }
  }

  @Override
  @Async
  public void createProductL3AuditLog(String businessPartnerCode, String gdnSku, String productSku, String name,
      String activity, String oldValue, String newValue, boolean needRevision, String pickupPointCode)
      throws JsonProcessingException {
    String requestId = mandatoryParameterHelper.getRequestId();
    String changedBy = mandatoryParameterHelper.getUsername();
    String clientHost = mandatoryParameterHelper.getClientId();
    UpdatedProductHistory auditLog = UpdatedProductHistory.builder()
        .requestId(Objects.nonNull(requestId) ? requestId : UUID.randomUUID().toString())
        .changedBy(Objects.nonNull(changedBy) ? changedBy : DEFAULT_USER)
        .clientHost(Objects.nonNull(clientHost) ? clientHost : CLIENT_HOST_KEY)
        .accessTime(Calendar.getInstance().getTime()).activity(activity).productSku(productSku).gdnSku(gdnSku)
        .gdnName(name).businessPartnerCode(businessPartnerCode).status(Boolean.TRUE).oldValues(oldValue)
        .newValues(newValue).build();
    if (needRevision) {
      auditLog.setActivity(auditLog.getActivity() + Constants.NEED_REVISION);
    }
    Map<String, String> itemSkuAndPickupPointCodeMap = null;
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    updatedProductHistoryList.add(auditLog);
    try {
      if (StringUtils.isBlank(pickupPointCode)) {
        GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = this.xProductOutbound
            .getItemPickupPointCodeByItemSkus(ConverterUtil.toSimpleListStringRequest(updatedProductHistoryList));
        itemSkuAndPickupPointCodeMap =
            ConverterUtil.toItemSkuAndPickupPointCodeMap(itemPickupPointCodeByItemSkus.getContent());
        auditLog.setPickupPointCode(itemSkuAndPickupPointCodeMap.getOrDefault(auditLog.getGdnSku(), Constants.HYPHEN));
      } else {
        auditLog.setPickupPointCode(pickupPointCode);
      }
      auditLog.setOnlineStatus(true);
    } catch (Exception e) {
      LOGGER.error("Exception caught while updating auditLog, Error Message : {} , Exception : {} ", e.getMessage(), e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, e.getMessage());
    }
    if (productHistoryUpdateThroughEvent) {
      publishProductUpdateHistoryEvent(auditLog);
    } else {
      createAudit(Collections.singletonList(auditLog), false);
    }

  }

  @Override
  public Page<UpdatedProductHistory> getOfflineProductHistoryByItemSkuAndPickupPointCode(String storeId,
      String itemSku, String pickupPointCode, Pageable pageable) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku), Constants.ITEM_SKU_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointCode),
        Constants.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return updatedProductHistoryRepository.findByGdnSkuAndPickupPointCodeAndOnlineStatusFalseOrderByAccessTimeDesc(
        itemSku, pickupPointCode, pageable);
  }

  @Override
  public Page<HistoryUpdateResponse> getProductUpdateHistoryByAuditTrailId(List<String> auditTrailIds,
    int page, int size, long totalElements) {
    List<UpdatedProductHistory> updatedProductHistories =
      updatedProductHistoryRepository.findByAuditTrailIdInOrderByAccessTimeDesc(auditTrailIds);
    List<HistoryUpdateResponse> historyResponseList =
      ConverterUtil.toHistoryUpdateResponseList(updatedProductHistories);
    return new PageImpl<>(historyResponseList, PageRequest.of(page, size), totalElements);
  }

  @Override
  public Page<HistoryUpdateResponse> findProductUpdateHistoryByProductSkuAndKeyword(
          HistoryUpdateRequest historyUpdateRequest, int page, int size) {
    Page<UpdatedProductHistory> updatedProductHistories =
      updatedProductHistoryCustomRepository.findByHistoryUpdateRequest(historyUpdateRequest,
              PageRequest.of(page, size));
    List<HistoryUpdateResponse> historyUpdateResponseList =
      ConverterUtil.toHistoryUpdateResponseList(updatedProductHistories.getContent());
    return new PageImpl<>(historyUpdateResponseList, PageRequest.of(page, size),
      updatedProductHistories.getTotalElements());
  }

  @Override
  public void updateProductHistoryDeltailList(List<UpdatedProductHistory> auditTrailUpdatedProductList)
      throws JsonProcessingException {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = this.xProductOutbound
        .getItemPickupPointCodeByItemSkus(ConverterUtil.toSimpleListStringRequest(auditTrailUpdatedProductList));
    Map<String, String> itemSkuAndPickupPointCodeMap =
        ConverterUtil.toItemSkuAndPickupPointCodeMap(itemPickupPointCodeByItemSkus.getContent());
    auditTrailUpdatedProductList =
        CommonUtils.setOnlineStatusAndPickupPointCode(auditTrailUpdatedProductList, itemSkuAndPickupPointCodeMap);
    if (productHistoryUpdateThroughEvent) {
      auditTrailUpdatedProductList.forEach(this::publishProductUpdateHistoryEvent);
    } else {
      createAudit(auditTrailUpdatedProductList, false);
    }
  }

  private UpdatedProductHistoryService getUpdatedProductHistoryServiceBean() {
    return applicationContext.getBean(UpdatedProductHistoryService.class);
  }

  @Override
  public UpdatedProductHistory addToUpdatedProductHistory(String gdnSku, String activity, String businessPartnerCode,
      String requestId, String username, String oldValues, String newValues, String productSku, String itemName,
      String pickupPointcode, boolean onlineStatus) {
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setGdnSku(gdnSku);
    updatedProductHistory.setActivity(activity);
    updatedProductHistory.setAccessTime(new Date());
    updatedProductHistory.setBusinessPartnerCode(businessPartnerCode);
    updatedProductHistory.setRequestId(requestId);
    updatedProductHistory.setChangedBy(username);
    updatedProductHistory.setOldValues(oldValues);
    updatedProductHistory.setNewValues(newValues);
    updatedProductHistory.setGdnName(itemName);
    updatedProductHistory.setProductSku(productSku);
    updatedProductHistory.setPickupPointCode(pickupPointcode);
    updatedProductHistory.setOnlineStatus(onlineStatus);
    return updatedProductHistory;
  }

  @Override
  public void deleteUpdateProductHistoryByStoreIdAndProductId(String storeId, String productId){
    updatedProductHistoryRepository.deleteByStoreIdAndProductId(storeId, productId);
  }
}
