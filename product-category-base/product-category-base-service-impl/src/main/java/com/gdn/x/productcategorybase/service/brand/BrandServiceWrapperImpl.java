package com.gdn.x.productcategorybase.service.brand;

import java.util.Arrays;
import java.util.Collections;

import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.dto.BrandCreationDTO;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.service.AttributeService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandServiceWrapperResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.util.ConverterUtil;

@Service
public class BrandServiceWrapperImpl implements BrandServiceWrapper {

  @Autowired
  private BrandWipService brandWipService;

  @Autowired
  private SolrBrandRepository solrBrandRepository;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;


  @Autowired
  private BrandWipHistoryService brandWipHistoryService;

  @Autowired
  private AttributeService attributeService;

  @Override
  public CreateBrandResponse approveBrand(BrandApproveRequest brandApproveRequest) throws Exception {
    BrandServiceWrapperResponse response = brandWipService.approveBrand(brandApproveRequest);
    solrBrandRepository.addBrandsToBrandCollectionSolr(
        Arrays.asList(ConverterUtil.generateSolrBrandModelApproveExistingBrand(response.getBrandWip())));
    if (response.isUnDeleteBrand()) {
      domainEventPublisherService.publishBrandUpdated(response.getBrand());
    }
    this.applicationCacheServiceBean
        .deleteAllBrandCache(response.getBrand().getStoreId(), response.getBrand().getBrandCode(),
            response.getBrand().getBrandName());
    if (brandApproveRequest.isProtectedBrand()) {
      this.applicationCacheServiceBean.evictProtectedBrandCache(response.getBrand().getStoreId());
    }
    return new CreateBrandResponse(response.getBrandWip().getBrandCode(), response.getBrandWip().getBrandLogoPath(),
        response.getBrandWip().getProfileBannerPath(), response.getBrandWip().getBrandName());
  }

  @Override
  public BrandWipResponse rejectBrand(BrandRejectRequest brandRejectRequest) throws Exception {
    BrandWipResponse brandWipResponse = brandWipService.rejectBrand(brandRejectRequest);
    this.solrBrandRepository.deleteBrandsFromBrandCollectionSolr(Arrays.asList(brandWipResponse.getId()));
    this.applicationCacheServiceBean.deleteAllBrandCache(brandWipResponse.getStoreId(),
        StringUtils.isBlank(brandWipResponse.getBrandCode()) ?
            brandWipResponse.getBrandRequestCode() :
            brandWipResponse.getBrandCode(), brandWipResponse.getBrandName());
    if(brandWipResponse.isProtectedBrand()){
    this.applicationCacheServiceBean.evictProtectedBrandCache(brandWipResponse.getStoreId());
    }
    return brandWipResponse;
  }

  @Override
  public void updateBrand(String storeId, BrandApproveRequest brandApproveRequest) throws Exception {
    BrandWip brandWip = brandWipService.update(storeId, brandApproveRequest);
    this.solrBrandRepository.updateBrandNameInSolr(brandWip.getId(), brandWip.getBrandName(), brandWip.isProtectedBrand());
  }

  @Override
  public String createBrand(String storeId, BrandWip brandWip) throws Exception {
    BrandCreationDTO brandCreationDTO = brandWipService.create(storeId, brandWip);
    SolrBrandModel solrBrandModel =
      ConverterUtil.generateSolrBrandModel(brandCreationDTO.getBrandWip());
    this.domainEventPublisherService.publishSolrAddBrandEvent(
      SolrAddBrandListDomainEventModel.builder()
        .solrBrandModels(Collections.singletonList(solrBrandModel)).build());
    domainEventPublisherService.publishBrandCreated(brandWip);
    if (brandCreationDTO.getBrandWip().isProtectedBrand()) {
      applicationCacheServiceBean.evictProtectedBrandCache(brandWip.getStoreId());
    }
    domainEventPublisherService.publishBrandHistory(ConverterUtil.toBrandWipHistoryModel(brandWip,
        GdnMandatoryParameterUtil.getUsername()));
    this.attributeService.evictAttributeCache(storeId, brandCreationDTO.getAttribute().getId(),
      brandCreationDTO.getAttribute().getAttributeCode());
    return brandCreationDTO.getBrandWip().getBrandRequestCode();
  }
}
