package com.gdn.x.productcategorybase.service.brand;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;

import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.dto.BrandCreationDTO;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.service.AttributeService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandServiceWrapperResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;

public class BrandServiceWrapperImplTest {

  private static final String ID = "id";
  private static final String DEFAULT_BRAND_NAME = "BliBli";
  private static final String DEFAULT_STORE_ID = "BliBli";
  private static final String DEFAULT_BRAND_CODE = "BR-0001";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BRD-0001-0001";
  private static final String DEFAULT_DESCRIPTION = "In review";
  private static final String DEFAULT_DESCRIPTION_REJECT = "Rejected";
  private static final String DEFAULT_NOTES = "NOTES";
  private static final String DRAFT = "DRAFT";
  private BrandApproveRequest brandApproveRequest;
  private BrandServiceWrapperResponse brandServiceWrapperResponse;
  private BrandWip brandWip;
  private Brand brand;
  private BrandRejectRequest brandRejectRequest;
  private BrandWipResponse brandWipResponse;

  @InjectMocks
  private BrandServiceWrapperImpl brandServiceWrapper;

  @Mock
  private BrandWipService brandWipService;

  @Mock
  private SolrBrandRepository solrBrandRepository;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;


  @Mock
  private BrandWipHistoryService brandWipHistoryService;

  @Mock
  private AttributeService attributeService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandApproveRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);

    brandWip = new BrandWip();
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    brandWip.setStoreId(DEFAULT_STORE_ID);
    brandWip.setBrandCode(DEFAULT_BRAND_CODE);
    brandWip.setBrandDescription(DEFAULT_DESCRIPTION.getBytes());
    brandWip.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWip.setState(BrandWipState.DRAFT);

    brand = new Brand();
    brand.setBrandName(DEFAULT_BRAND_NAME);
    brand.setStoreId(DEFAULT_STORE_ID);
    brand.setBrandCode(DEFAULT_BRAND_CODE);
    brand.setBrandDescription(DEFAULT_DESCRIPTION.getBytes());

    brandServiceWrapperResponse = new BrandServiceWrapperResponse();
    brandServiceWrapperResponse.setBrandWip(brandWip);
    brandServiceWrapperResponse.setBrand(brand);

    brandRejectRequest = new BrandRejectRequest();
    brandRejectRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);

    brandWipResponse = new BrandWipResponse();
    brandWipResponse.setId(ID);
    brandWipResponse.setStoreId(DEFAULT_STORE_ID);
    brandWipResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandWipResponse.setBrandName(DEFAULT_BRAND_NAME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(brandWipService, solrBrandRepository, domainEventPublisherService,
        applicationCacheServiceBean);
  }

  @Test
  public void approveBrandUndeleteTest() throws Exception {
    brandServiceWrapperResponse.setUnDeleteBrand(true);
    brandApproveRequest.setProtectedBrand(true);
    doNothing().when(applicationCacheServiceBean)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    Mockito.when(brandWipService.approveBrand(brandApproveRequest)).thenReturn(brandServiceWrapperResponse);
    CreateBrandResponse createBrandResponse = brandServiceWrapper.approveBrand(brandApproveRequest);
    Mockito.verify(brandWipService).approveBrand(brandApproveRequest);
    Mockito.verify(solrBrandRepository).addBrandsToBrandCollectionSolr(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    Mockito.verify(domainEventPublisherService).publishBrandUpdated(Mockito.any(Brand.class));
    Mockito.verify(applicationCacheServiceBean).evictProtectedBrandCache(brandServiceWrapperResponse.getBrand().getStoreId());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, createBrandResponse.getBrandCode());
  }

  @Test
  public void approveBrandTest() throws Exception {
    brandServiceWrapperResponse.getBrandWip().setBrandName(DEFAULT_BRAND_NAME);
    Mockito.when(brandWipService.approveBrand(brandApproveRequest)).thenReturn(brandServiceWrapperResponse);
    doNothing().when(applicationCacheServiceBean)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    CreateBrandResponse createBrandResponse = brandServiceWrapper.approveBrand(brandApproveRequest);
    Mockito.verify(brandWipService).approveBrand(brandApproveRequest);
    Mockito.verify(solrBrandRepository).addBrandsToBrandCollectionSolr(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, createBrandResponse.getBrandCode());
    Assertions.assertEquals(DEFAULT_BRAND_NAME, createBrandResponse.getBrandName());
  }

  @Test
  public void rejectBrandTest() throws Exception {
    brandWipResponse.setProtectedBrand(false);
    brandWipResponse.setState(DEFAULT_DESCRIPTION_REJECT);
    Mockito.when(brandWipService.rejectBrand(brandRejectRequest)).thenReturn(brandWipResponse);
    doNothing().when(applicationCacheServiceBean)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    BrandWipResponse response = brandServiceWrapper.rejectBrand(brandRejectRequest);
    Mockito.verify(brandWipService).rejectBrand(brandRejectRequest);
    Mockito.verify(solrBrandRepository).deleteBrandsFromBrandCollectionSolr(Arrays.asList(ID));
    Mockito.verify(applicationCacheServiceBean)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    Assertions.assertEquals(ID, response.getId());
  }

  @Test
  public void rejectProtectedBrandTest() throws Exception {
    brandWipResponse.setProtectedBrand(true);
    brandWipResponse.setState(DRAFT);
    Mockito.when(brandWipService.rejectBrand(brandRejectRequest)).thenReturn(brandWipResponse);
    doNothing().when(applicationCacheServiceBean)
      .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    BrandWipResponse response = brandServiceWrapper.rejectBrand(brandRejectRequest);
    Mockito.verify(brandWipService).rejectBrand(brandRejectRequest);
    Mockito.verify(solrBrandRepository).deleteBrandsFromBrandCollectionSolr(Arrays.asList(ID));
    Mockito.verify(applicationCacheServiceBean)
      .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    Mockito.verify(applicationCacheServiceBean).evictProtectedBrandCache(brandWipResponse.getStoreId());
    Assertions.assertEquals(ID, response.getId());
  }

  @Test
  public void updateBrandTest() throws Exception {
    brandWip.setId(ID);
    Mockito.when(brandWipService.update(DEFAULT_STORE_ID, brandApproveRequest)).thenReturn(brandWip);
    Mockito.doNothing().when(this.solrBrandRepository).updateBrandNameInSolr(ID, DEFAULT_BRAND_NAME,
        brandWip.isProtectedBrand());
    brandServiceWrapper.updateBrand(DEFAULT_STORE_ID, brandApproveRequest);
    Mockito.verify(brandWipService).update(DEFAULT_STORE_ID, brandApproveRequest);
    Mockito.verify(solrBrandRepository).updateBrandNameInSolr(ID, DEFAULT_BRAND_NAME, brandWip.isProtectedBrand());
  }

  @Test
  void createBrandTest() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    Mockito.when(brandWipService.create(DEFAULT_STORE_ID, brandWip))
      .thenReturn(BrandCreationDTO.builder().brandWip(brandWip).attribute(attribute).build());
    String brandRequestCode = brandServiceWrapper.createBrand(DEFAULT_STORE_ID, brandWip);
    Mockito.verify(brandWipService).create(DEFAULT_STORE_ID, brandWip);
    Mockito.verify(domainEventPublisherService)
      .publishSolrAddBrandEvent(Mockito.any(SolrAddBrandListDomainEventModel.class));
    Mockito.verify(this.domainEventPublisherService).publishBrandCreated(brandWip);
    Mockito.verify(this.domainEventPublisherService)
      .publishBrandHistory(Mockito.any(BrandHistoryEventModel.class));
    Mockito.verify(this.attributeService)
      .evictAttributeCache(DEFAULT_STORE_ID, attribute.getId(), attribute.getAttributeCode());
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, brandRequestCode);
  }

  @Test
  void createProtectedBrandTest() throws Exception {
    brandWip.setProtectedBrand(true);
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    Mockito.when(brandWipService.create(DEFAULT_STORE_ID, brandWip))
      .thenReturn(BrandCreationDTO.builder().brandWip(brandWip).attribute(attribute).build());
    String brandRequestCode = brandServiceWrapper.createBrand(DEFAULT_STORE_ID, brandWip);
    Mockito.verify(brandWipService).create(DEFAULT_STORE_ID, brandWip);
    Mockito.verify(domainEventPublisherService)
      .publishSolrAddBrandEvent(Mockito.any(SolrAddBrandListDomainEventModel.class));
    Mockito.verify(this.domainEventPublisherService).publishBrandCreated(brandWip);
    Mockito.verify(this.domainEventPublisherService)
      .publishBrandHistory(Mockito.any(BrandHistoryEventModel.class));
    Mockito.verify(this.attributeService)
      .evictAttributeCache(DEFAULT_STORE_ID, attribute.getId(), attribute.getAttributeCode());
    Mockito.verify(applicationCacheServiceBean).evictProtectedBrandCache(brandWip.getStoreId());
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, brandRequestCode);
  }
}