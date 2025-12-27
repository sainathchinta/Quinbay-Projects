package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryCount;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorInventoryCriteria;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorState;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.repository.agregator.ProductLevel3AggregatorRepository;

@Service
@Transactional(readOnly = true)
public class ProductLevel3AggregatorServiceBean implements ProductLevel3AggregatorService {
  
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3AggregatorServiceBean.class);

  private static final String GDN_SKUS_CANNOT_EMPTY = "GdnSkus cannot be empty";
  
  @Autowired
  protected ProductLevel3AggregatorRepository productLevel3AggregatorRepository;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Lazy
  @Autowired
  @Qualifier(value = "ProductLevel3ServiceBeanOld")
  private ProductLevel3Service productLevel3Service;
  
  @Override
  public ProductLevel3Aggregator findByGdnSku(String gdnSku) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    return this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(storeId, gdnSku);
  }

  @Override
  public Page<ProductLevel3Aggregator> findByBusinessPartnerCodeAndInventoryFilterAndState(String businessPartnerCode,
      ProductLevel3AggregatorInventoryCriteria inventoryCriteria, ProductLevel3AggregatorState state, Pageable pageable)
      throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    Page<ProductLevel3Aggregator> productLevel3Aggregators = null;
    if (ProductLevel3AggregatorInventoryCriteria.AVAILABLE.equals(inventoryCriteria)) {
      productLevel3Aggregators =
          this.productLevel3AggregatorRepository
              .findByStoreIdAndBusinessPartnerCodeAndStateAndOosAndMarkForDeleteFalse(storeId, businessPartnerCode,
                  state, false, pageable);
    } else if (ProductLevel3AggregatorInventoryCriteria.OOS.equals(inventoryCriteria)) {
      productLevel3Aggregators = this.productLevel3AggregatorRepository
          .findByStoreIdAndBusinessPartnerCodeAndStateAndOosAndMarkForDeleteFalseOrderByUpdatedDateDesc(storeId,
              businessPartnerCode, state, true, pageable);
    } else if (ProductLevel3AggregatorInventoryCriteria.MINIMUM_STOCK.equals(inventoryCriteria)) {
      productLevel3Aggregators =
          this.productLevel3AggregatorRepository
              .findByStoreIdAndBusinessPartnerCodeAndStateAndMinimumStockAndOosAndMarkForDeleteFalse(storeId,
                  businessPartnerCode, state, true, false, pageable);
    } else {
      productLevel3Aggregators = new PageImpl<ProductLevel3Aggregator>(new ArrayList<>(), pageable, 0);
    }
    return productLevel3Aggregators;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class,
      propagation = Propagation.REQUIRES_NEW)
  public void updateOOS(Level2InventoryOosEvent messageEvent, int retry) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3Aggregator productLevel3Aggregator = this.productLevel3AggregatorRepository
        .findByStoreIdAndGdnSkuAndMarkForDeleteFalse(storeId, messageEvent.getLevel2Id());
    LOGGER.info("updateMinimumStock with param : {},{}", messageEvent, retry);
    if(retry < 3){
      try {
        if (productLevel3Aggregator == null) {
          ProductLevel3Summary productLevel3Summary =
              this.productLevel3Service.findSummaryByGdnSku(messageEvent.getLevel2MerchantCode(), messageEvent.getLevel2Id());
          productLevel3Aggregator = new ProductLevel3Aggregator();
          productLevel3Aggregator.setStoreId(storeId);
          productLevel3Aggregator.setBusinessPartnerCode(messageEvent.getLevel2MerchantCode());
          productLevel3Aggregator.setGdnSku(messageEvent.getLevel2Id());
          productLevel3Aggregator.setEventTimestamp(new Date(messageEvent.getTimestamp()));
          productLevel3Aggregator.setState(productLevel3Summary.getIsArchived() ? ProductLevel3AggregatorState.ARCHIVED : ProductLevel3AggregatorState.ACTIVE);
          this.checkSyncStockAndSetOos(productLevel3Summary, productLevel3Aggregator);
          this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
        } else {
          if (productLevel3Aggregator.getEventTimestamp() == null || messageEvent.getTimestamp() >= productLevel3Aggregator.getEventTimestamp().getTime()) {
            productLevel3Aggregator.setOos(true);
            productLevel3Aggregator.setMinimumStock(false);
            productLevel3Aggregator.setEventTimestamp(new Date(messageEvent.getTimestamp()));
            this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
          } else {
            LOGGER.info("updateOOS productLevel3Aggregator is up to date with message : {}", messageEvent);
          }
        }
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductLevel3AggregatorServiceBean.updateOOS ! Retry again", ofe);
        LOGGER.info("ProductLevel3AggregatorServiceBean.updateOOS retry again with value : {},{}", messageEvent, retry + 1);
        updateOOS(messageEvent, retry + 1);
      }
    }
  }
  
  @Override
  public void updateNonOOS(Level2InventoryNonOosEvent messageEvent, int retry) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3Aggregator productLevel3Aggregator = this.productLevel3AggregatorRepository
        .findByStoreIdAndGdnSkuAndMarkForDeleteFalse(storeId, messageEvent.getLevel2Id());
    LOGGER.info("updateMinimumStock with param : {},{}", messageEvent, retry);
    if(retry < 3){
      try {
        if (productLevel3Aggregator == null) {
          ProductLevel3Summary productLevel3Summary =
              this.productLevel3Service.findSummaryByGdnSku(messageEvent.getLevel2MerchantCode(), messageEvent.getLevel2Id());
          productLevel3Aggregator = new ProductLevel3Aggregator();
          productLevel3Aggregator.setStoreId(storeId);
          productLevel3Aggregator.setBusinessPartnerCode(messageEvent.getLevel2MerchantCode());
          productLevel3Aggregator.setGdnSku(messageEvent.getLevel2Id());
          productLevel3Aggregator.setState(productLevel3Summary.getIsArchived() ? ProductLevel3AggregatorState.ARCHIVED : ProductLevel3AggregatorState.ACTIVE);
          this.checkSyncStockAndSetOos(productLevel3Summary, productLevel3Aggregator);
          productLevel3Aggregator.setEventTimestamp(new Date(messageEvent.getTimestamp()));
          this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
        } else {
          if (productLevel3Aggregator.getEventTimestamp() == null || messageEvent.getTimestamp() >= productLevel3Aggregator.getEventTimestamp().getTime()) {
            productLevel3Aggregator.setOos(false);
            productLevel3Aggregator.setEventTimestamp(new Date(messageEvent.getTimestamp()));
            this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
          } else {
            LOGGER.info("updateNonOOS productLevel3Aggregator is up to date with message : {}", messageEvent);
          }
        }
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductLevel3AggregatorServiceBean.updateNonOOS ! Retry again", ofe);
        LOGGER.info("ProductLevel3AggregatorServiceBean.updateOOS retry again with value : {},{}", messageEvent, retry + 1);
        updateNonOOS(messageEvent, retry + 1);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void updateMinimumStock(Level2InventoryMinimumStockAlertEvent messageEvent, boolean value, int retry)
      throws Exception {
    LOGGER.info("updateMinimumStock with param : {},{},{},{}", messageEvent, value, retry);
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3Aggregator productLevel3Aggregator = this.productLevel3AggregatorRepository
        .findByStoreIdAndGdnSkuAndMarkForDeleteFalse(storeId, messageEvent.getGdnSku());
    if(retry < 3){
      try {
        if (productLevel3Aggregator == null) {
          ProductLevel3Summary productLevel3Summary =
              this.productLevel3Service.findSummaryByGdnSku(messageEvent.getBusinessPartnerCode(), messageEvent.getGdnSku());
          productLevel3Aggregator = new ProductLevel3Aggregator();
          productLevel3Aggregator.setStoreId(storeId);
          productLevel3Aggregator.setBusinessPartnerCode(messageEvent.getBusinessPartnerCode());
          productLevel3Aggregator.setGdnSku(messageEvent.getGdnSku());
          productLevel3Aggregator.setState(productLevel3Summary.getIsArchived() ? ProductLevel3AggregatorState.ARCHIVED : ProductLevel3AggregatorState.ACTIVE);
          this.checkSyncStockAndSetOos(productLevel3Summary, productLevel3Aggregator);
          productLevel3Aggregator.setEventTimestamp(new Date(messageEvent.getTimestamp()));
          this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
        } else {
          if (productLevel3Aggregator.getEventTimestamp() == null || messageEvent.getTimestamp() >= productLevel3Aggregator.getEventTimestamp().getTime()) {
            productLevel3Aggregator.setMinimumStock(value);
            productLevel3Aggregator.setOos(!(messageEvent.getAvailableStock() > 0));
            productLevel3Aggregator.setEventTimestamp(new Date(messageEvent.getTimestamp()));
            this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
          } else {
            LOGGER.info("updateMinimumStock productLevel3Aggregator is up to date with message : {}", messageEvent);
          }
        }
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductLevel3AggregatorServiceBean.updateMinimumStock ! Retry again", ofe);
        LOGGER.info("ProductLevel3AggregatorServiceBean.updateMinimumStock retry again with value : {},{},{}", messageEvent, value, retry+1);
        updateMinimumStock(messageEvent, value, retry+1);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void update(String gdnSku, String businessPartnerCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3Aggregator productLevel3Aggregator =
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(storeId, gdnSku);
    if (productLevel3Aggregator == null) {
      this.create(gdnSku, businessPartnerCode);
    } else {
      ProductLevel3Summary productLevel3Summary =
          this.productLevel3Service.findSummaryByGdnSku(businessPartnerCode, gdnSku);
      productLevel3Aggregator.setState(productLevel3Summary.getIsArchived() ? ProductLevel3AggregatorState.ARCHIVED
          : ProductLevel3AggregatorState.ACTIVE);
      
      this.checkSyncStockAndSetOos(productLevel3Summary, productLevel3Aggregator);
      this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void create(String gdnSku, String businessPartnerCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3Summary productLevel3Summary =
        this.productLevel3Service.findSummaryByGdnSku(businessPartnerCode, gdnSku);
    ProductLevel3Aggregator productLevel3Aggregator = new ProductLevel3Aggregator();
    productLevel3Aggregator.setStoreId(storeId);
    productLevel3Aggregator.setBusinessPartnerCode(businessPartnerCode);
    productLevel3Aggregator.setGdnSku(gdnSku);
    productLevel3Aggregator.setState(productLevel3Summary.getIsArchived() ? ProductLevel3AggregatorState.ARCHIVED : ProductLevel3AggregatorState.ACTIVE);
    
    this.checkSyncStockAndSetOos(productLevel3Summary, productLevel3Aggregator);
    this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
  }
  
  private void checkSyncStockAndSetOos(ProductLevel3Summary productLevel3Summary, ProductLevel3Aggregator productLevel3Aggregator){
    if(productLevel3Summary.getSynchronizeStock()){
      productLevel3Aggregator.setOos(!(productLevel3Summary.getAvailableStockLevel1() > 0));
      productLevel3Aggregator.setMinimumStock(!(productLevel3Summary.getAvailableStockLevel1() > productLevel3Summary
          .getMinimumStockLevel2() || productLevel3Aggregator.getOos()));
    } else {
      productLevel3Aggregator.setOos(!(productLevel3Summary.getAvailableStockLevel2() > 0));
      productLevel3Aggregator.setMinimumStock(!(productLevel3Summary.getAvailableStockLevel2() > productLevel3Summary
          .getMinimumStockLevel2() || productLevel3Aggregator.getOos()));
    }
  }

  @Override
  public ProductLevel3SummaryCount countSummary(String businessPartnerCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3SummaryCount summaryCount = new ProductLevel3SummaryCount();
    summaryCount.setBusinessPartnerCode(businessPartnerCode);
    List<Object[]> oosList =
        this.productLevel3AggregatorRepository.countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
            storeId, businessPartnerCode);
   Map<ProductLevel3InventoryCriteria, Long> mapResult = new HashMap<>();
    if (CollectionUtils.isNotEmpty(oosList)) {
      Map<Boolean, Long> oosMap =
          oosList.stream().collect(Collectors.toMap(oos -> Boolean.valueOf(String.valueOf(oos[0])), oos -> (Long) oos[1]));
      long available = oosMap.getOrDefault(Boolean.FALSE, 0L);
      long oos = oosMap.getOrDefault(Boolean.TRUE, 0L);
      long stockAlert = this.productLevel3AggregatorRepository.countSummaryMinimumStockByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
          storeId, businessPartnerCode);
      Long oosL3Count =
          xProductOutbound.getProductCountByType(businessPartnerCode, Constants.TYPE_OF_PRODUCT).getOutOfStock();
      mapResult.put(ProductLevel3InventoryCriteria.AVAILABLE, available);
      mapResult.put(ProductLevel3InventoryCriteria.OOS, oosL3Count);
      mapResult.put(ProductLevel3InventoryCriteria.STOCK_ALERT, stockAlert);
      summaryCount.setTotalCounts(oos + available);
      summaryCount.setStockConditionCounts(mapResult);
    } else {
      mapResult.put(ProductLevel3InventoryCriteria.AVAILABLE, 0L);
      mapResult.put(ProductLevel3InventoryCriteria.OOS, 0L);
      mapResult.put(ProductLevel3InventoryCriteria.STOCK_ALERT, 0L);
      summaryCount.setTotalCounts(0L);
      summaryCount.setStockConditionCounts(mapResult);
    }
    return summaryCount;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void create(ProductLevel3Aggregator productLevel3Aggregator) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    productLevel3Aggregator.setStoreId(storeId);
    this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void updateState(String gdnSku, String businessPartnerCode, ProductLevel3AggregatorState state) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3Aggregator productLevel3Aggregator =
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(storeId, gdnSku);
    if (productLevel3Aggregator == null) {
      this.create(gdnSku, businessPartnerCode);
    } else {
      productLevel3Aggregator.setState(state);
      this.productLevel3AggregatorRepository.save(productLevel3Aggregator);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void delete(String storeId, List<String> gdnSkus) throws Exception {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(gdnSkus), GDN_SKUS_CANNOT_EMPTY);
    List<ProductLevel3Aggregator> productLevel3AggregatorList =
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuInAndMarkForDeleteFalse(storeId, gdnSkus);
    if (CollectionUtils.isNotEmpty(productLevel3AggregatorList)) {
      for (ProductLevel3Aggregator productLevel3Aggregator : productLevel3AggregatorList) {
        productLevel3Aggregator.setMarkForDelete(true);
      }
      this.productLevel3AggregatorRepository.saveAll(productLevel3AggregatorList);
    }
  }
}
