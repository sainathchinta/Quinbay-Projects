package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.BulkDataForRecategorizationRequest;
import com.gda.mta.product.dto.CategoryProductCodeMappingRequest;
import com.gda.mta.product.dto.CategoryProductSkuMappingRequest;
import com.gda.mta.product.dto.CategoryUserMappingRequest;
import com.gda.mta.product.dto.ProductSkuToSalesCatalogMappingRequest;
import com.gda.mta.product.dto.RecategorizationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.config.RecategorizationStatus;
import com.gdn.mta.product.entity.CategoryProductCodeMapping;
import com.gdn.mta.product.entity.CategoryProductSkuMapping;
import com.gdn.mta.product.entity.CategoryUserMapping;
import com.gdn.mta.product.entity.ProductSkuSalesCatalogMapping;
import com.gdn.mta.product.entity.Recategorization;
import com.gdn.mta.product.repository.CategoryProductCodeMappingRepository;
import com.gdn.mta.product.repository.CategoryProductSkuMappingRepository;
import com.gdn.mta.product.repository.CategoryUserMappingRepository;
import com.gdn.mta.product.repository.ProductSkuSalesCatalogMappingRepository;
import com.gdn.mta.product.repository.RecategorizationRepository;
import com.gdn.partners.pbp.service.tools.ProductCollectionToolsService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 10/05/18.
 */

@Service
@Transactional(readOnly = true, rollbackFor = Exception.class)
public class RecategorizationServiceBean implements RecategorizationService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RecategorizationServiceBean.class);

  @Autowired
  private RecategorizationRepository recategorizationRepository;

  @Autowired
  private CategoryUserMappingRepository categoryUserMappingRepository;

  @Autowired
  private CategoryProductCodeMappingRepository categoryProductCodeMappingRepository;

  @Autowired
  private CategoryProductSkuMappingRepository categoryProductSkuMappingRepository;

  @Autowired
  private ProductSkuSalesCatalogMappingRepository productSkuSalesCatalogMappingRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductCollectionToolsService productCollectionToolsService;

  @Override
  @Transactional(rollbackFor = Exception.class)
  public String save(RecategorizationRequest recategorizationRequest) throws Exception {
    Recategorization recategorization = new Recategorization();
    convertProductRequestToRecategorization(recategorizationRequest, recategorization);
    String id = null;
    Recategorization savedRecategorization = findById(recategorization.getId());
    if (null == savedRecategorization) {
      recategorization.setCreatedBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
      recategorization.setCreatedDate(new Date());
      id = recategorizationRepository.save(recategorization).getId();
    } else {
      savedRecategorization.setUpdatedBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
      savedRecategorization.setUpdatedDate(new Date());
      savedRecategorization.setStatus(recategorizationRequest.getStatus());
      id = recategorizationRepository.save(savedRecategorization).getId();
    }
    return id;
  }



  private void convertProductRequestToRecategorization(RecategorizationRequest request,
      Recategorization recategorization) {
    recategorization.setId(request.getId());
    recategorization.setName(request.getName());
    recategorization.setStatus(request.getStatus());
    recategorization.setExcelFilePath(request.getExcelFilePath());
    recategorization.setMarkForDelete(request.isMarkForDelete());
    recategorization.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
  }

  @Trace(dispatcher=true)
  @Override
  @Async
  @Transactional(rollbackFor = Exception.class)
  public void processCategoryToProductCodeMapping(BulkDataForRecategorizationRequest recategorizationRequest) {
    String recatId = recategorizationRequest.getRecatId();
    for (List<Object> row : recategorizationRequest.getRowsData()) {
      CategoryProductCodeMappingRequest mappingRequest;
      try {
        productCollectionToolsService.moveProductCollectionCategory(recategorizationRequest.getRequestId(),
            recategorizationRequest.getUsername(), recategorizationRequest.getStoreId(), row.get(1).toString(),
            row.get(0).toString());
        mappingRequest =
            convertRowDataToCategoryProductCodeMappingRequest(row, recatId, RecategorizationStatus.COMPLETED.toString(),
                recategorizationRequest);
      } catch (Exception ex) {
        LOGGER.error(
            "error occurred while mapping categoryCode : {} to productCode : {} for recateId : {}, error is" + " : ",
            row.get(0), row.get(1), recatId, ex);
        mappingRequest =
            convertRowDataToCategoryProductCodeMappingRequest(row, recatId, RecategorizationStatus.FAILED.toString(),
                recategorizationRequest);
      }
      kafkaProducer.send(DomainEventName.CATEGORY_TO_PRODUCT_CODE_MAPPING_SAVE_EVENT, mappingRequest);
    }
  }

  private CategoryProductCodeMappingRequest convertRowDataToCategoryProductCodeMappingRequest(List<Object> row,
      String recatId, String status, BulkDataForRecategorizationRequest recategorizationRequest) {
    CategoryProductCodeMappingRequest mappingRequest = new CategoryProductCodeMappingRequest(row.get(0).toString(),
        row.get(1).toString(), status, recatId);
    mappingRequest.setStoreId(recategorizationRequest.getStoreId());
    mappingRequest.setRequestId(recategorizationRequest.getRequestId());
    mappingRequest.setUsername(recategorizationRequest.getUsername());
    return mappingRequest;
  }

  @Override
  public Recategorization findById(String recatId) throws Exception {
    return recategorizationRepository.findById(recatId).orElse(null);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void saveCategoryToProductCodeMapping(CategoryProductCodeMappingRequest saveRequest) throws Exception {
    Recategorization recategorization = findById(saveRequest.getRecatId());
    if (null != recategorization) {
      CategoryProductCodeMapping mapping = new CategoryProductCodeMapping(saveRequest.getRecatId(), saveRequest
          .getCategoryCode(), saveRequest.getProductCode(), saveRequest.getStatus(), saveRequest.getStoreId());
      mapping.setCreatedBy(saveRequest.getRequestId());
      mapping.setUpdatedBy(saveRequest.getRequestId());
      LOGGER.info("saving productCode to category mapping : {}", mapping);
      categoryProductCodeMappingRepository.save(mapping);
    } else {
      LOGGER.error("error while saving product code : {} to category : {} mapping in {}", saveRequest.getProductCode
          (), saveRequest.getCategoryCode(), CategoryProductCodeMapping.TABLE_NAME);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "error while saving product code and " +
          "category mapping in " + CategoryProductCodeMapping.TABLE_NAME);
    }
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void saveCategoryToProductSkuMapping(CategoryProductSkuMappingRequest saveRequest) throws Exception {
    Recategorization recategorization = findById(saveRequest.getRecatId());
    if (null != recategorization) {
      CategoryProductSkuMapping mapping = new CategoryProductSkuMapping(saveRequest.getRecatId(), saveRequest
          .getCategoryCode(), saveRequest.getProductSku(), saveRequest.getStatus(), saveRequest.getStoreId());
      mapping.setCreatedBy(saveRequest.getRequestId());
      mapping.setUpdatedBy(saveRequest.getRequestId());
      LOGGER.info("saving productSku to category mapping : {}", mapping);
      categoryProductSkuMappingRepository.save(mapping);
    } else {
      LOGGER.error("error while saving productSku : {} to category : {} mapping in {}", saveRequest.getProductSku(),
          saveRequest.getCategoryCode(), CategoryProductSkuMapping.TABLE_NAME);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "error while saving productSku and " +
          "category mapping in " + CategoryProductSkuMapping.TABLE_NAME);
    }
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void saveProductSkuToSalesCatalogMapping(ProductSkuToSalesCatalogMappingRequest saveRequest) throws Exception {
    Recategorization recategorization = findById(saveRequest.getRecatId());
    if (null != recategorization) {
      ProductSkuSalesCatalogMapping mapping = new ProductSkuSalesCatalogMapping(saveRequest.getRecatId(), saveRequest
          .getSalesCatalog(), saveRequest.getSalesCategory(), saveRequest.getProductSku(), saveRequest.getStatus(),
          saveRequest.getStoreId());
      mapping.setCreatedBy(saveRequest.getRequestId());
      mapping.setUpdatedBy(saveRequest.getRequestId());
      LOGGER.info("saving productSku to sales catalog mapping : {}", mapping);
      productSkuSalesCatalogMappingRepository.save(mapping);
    } else {
      LOGGER.error("error while saving productSku : {} to sales catalog : {} and sales category : {} mapping in {}",
          saveRequest.getProductSku(), saveRequest.getSalesCatalog(), saveRequest.getSalesCategory(),
          ProductSkuSalesCatalogMapping.TABLE_NAME);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "error while saving productSku and " +
          "sales catalog mapping in " + ProductSkuSalesCatalogMapping.TABLE_NAME);
    }
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void saveCategoryToUserMapping(CategoryUserMappingRequest saveRequest) throws Exception {
    Recategorization recategorization = findById(saveRequest.getRecatId());
    if (null != recategorization) {
      CategoryUserMapping mapping = new CategoryUserMapping(saveRequest.getRecatId(), saveRequest.getCategoryCode(),
          saveRequest.getBusinessPartnerCode(), saveRequest.getUserEmailId(), saveRequest.getStatus(), saveRequest
          .getStoreId());
      mapping.setCreatedBy(saveRequest.getRequestId());
      mapping.setUpdatedBy(saveRequest.getRequestId());
      LOGGER.info("saving user to category mapping : {}", mapping);
      categoryUserMappingRepository.save(mapping);
    } else {
      LOGGER.error("error while saving user : {} to category : {} mapping in {}", saveRequest.getUsername(),
          saveRequest.getCategoryCode(), CategoryUserMapping.TABLE_NAME);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "error while saving productSku and " +
          "category mapping in " + CategoryUserMapping.TABLE_NAME);
    }
  }
}
