package com.gdn.partners.pbp.service.tools;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3WipRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1SolrService;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.request.AttributeRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;

@Service
public class ProductCollectionToolsServiceBean implements ProductCollectionToolsService {
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductCollectionToolsServiceBean.class);
  
  @Autowired
  private ProductCollectionRepository productCollectionRepository;
  
  @Autowired
  private ProductLevel3WipRepository productLevel3WipRepository;
  
  @Autowired
  private ProductRepository productRepository;
  
  @Autowired
  private ProductLevel1SolrService prdLv1SolrService;
  
  @Autowired
  private ProductOutbound xProductOutbond;
  
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void syncState(List<String> productCodes, String specifiedState, String storeId, 
      String username) throws Exception {
    try{
      for(String productCode : productCodes){
        this.executeSyncState(productCode, specifiedState, storeId, username);
      }
    } catch(Exception e){
      throw e;
    }
  }
  
  @Override
  @Transactional(rollbackFor = Exception.class)
  public void moveProductCollectionCategory(String requestId, String username, String storeId, String productCode,
      String categoryCode) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    ProductCollection prdCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (prdCollection == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Product Collection " + productCode +
          " is not found");
    }

    GdnRestSingleResponse<CategorySummaryResponse> categorySummary = productRepository.movePrdCategoryByPrdCode
        (requestId, username, productCode, categoryCode);

    prdCollection.setCategoryCode(categorySummary.getValue().getCategoryCode());
    prdCollection.setCategoryName(categorySummary.getValue().getCategoryName());
    prdCollection.setUpdatedDate(new Date());
    productCollectionRepository.save(prdCollection);

    MasterCatalogRequest masterCatalogRequest = new MasterCatalogRequest();
    masterCatalogRequest.setCatalogCode(storeId);
    masterCatalogRequest.setCategory(new CategoryDTO(categoryCode, categoryCode));
    xProductOutbond.updateProductMasterCatalog(requestId, username, masterCatalogRequest, productCode);

    prdLv1SolrService.update(productCode);
  }
  
  private void executeSyncState(String productCode, String specifiedState, String storeId, 
      String username) throws Exception {
    ProductCollection productCollection = this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    
    if(productCollection == null){
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, 
          "Product collection is not found for: " + productCode + " and mfd false");
    }
    
    if(specifiedState.equals(productCollection.getState())){
      List<ProductLevel3Wip> productLevel3Wips =
          this.productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
              storeId, productCollection.getProductId());
      
      if(CollectionUtils.isNotEmpty(productLevel3Wips)){
        for(ProductLevel3Wip prdWip : productLevel3Wips){
          if(! prdWip.getState().equals(productCollection.getState())){
            prdWip.setState(productCollection.getState());
            prdWip.setUpdatedDate(new Date());
            prdWip.setUpdatedBy(username);
            productLevel3WipRepository.save(prdWip);
            LOGGER.info("ref=pnc-tools action=sync-state msg=Update state prd bp: {} with state={}", prdWip.getProductSku(), productCollection.getState());
          }
        }
      } else {
        LOGGER.warn("ref=pnc-tools action=sync-state msg=Product BP is not found for: "+productCollection.getProductId()+" and mfd = false");
      }
    } else {
      LOGGER.warn("ref=pnc-tools action=sync-state msg=Sync state is invalid, product collection state is: " + productCollection.getState() + " and specified state is: " + specifiedState);
    }
  }

  @Override
  public GdnBaseRestResponse addProductAttributesByProductCode(String requestId, String username,
      AddProductAttributesRequest request) throws Exception {
    GdnRestListResponse<ProductAttributeResponse> result =
        this.productRepository.addProductAttributesByProductCode(requestId, username, request);
    
    if(CollectionUtils.isNotEmpty(result.getContent())){
      for(ProductAttributeResponse prdAttrResp : result.getContent()){
        ProductAttributeRequest xproductAttrReq = new ProductAttributeRequest();
        xproductAttrReq.setAttribute(this.convertToAttributeReq(prdAttrResp.getAttribute()));
        xproductAttrReq.setOwnByProductItem(prdAttrResp.isOwnByProductItem());
        xproductAttrReq.setProductAttributeName(prdAttrResp.getProductAttributeName());
        xproductAttrReq.setProductAttributeValues(this.convertToPrdAttrValueReq(prdAttrResp.getProductAttributeValues()));
        xproductAttrReq.setSequence(prdAttrResp.getSequence());
        this.xProductOutbond.addProductAttribute(requestId, username, xproductAttrReq, 
            request.getProductCode());
      }
    }
    
    return new GdnBaseRestResponse(true);
  }

  private AttributeRequest convertToAttributeReq(AttributeResponse source){
    AttributeRequest attrReq = new AttributeRequest();
    BeanUtils.copyProperties(source, attrReq);
    return attrReq;
  }
  
  private List<ProductAttributeValueRequest> convertToPrdAttrValueReq(
      List<ProductAttributeValueResponse> source) {
    List<ProductAttributeValueRequest> result = new ArrayList<>();
    
    for(ProductAttributeValueResponse prdAttrValue : source){
      ProductAttributeValueRequest targetObj = new ProductAttributeValueRequest();
      BeanUtils.copyProperties(prdAttrValue, targetObj);
      result.add(targetObj);
    }
    return result;
  }
  
}
