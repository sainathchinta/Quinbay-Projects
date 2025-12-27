package com.gdn.partners.pcu.internal.web.config;

import com.gdn.partners.pcu.internal.service.AutoApprovedService;
import com.gdn.partners.pcu.internal.service.BrandAuthorisationService;
import com.gdn.partners.pcu.internal.service.BrandAuthorisationWipService;
import com.gdn.partners.pcu.internal.service.BulkInternalProcessService;
import com.gdn.partners.pcu.internal.service.CategoryHistoryService;
import com.gdn.partners.pcu.internal.service.FileHelper;
import com.gdn.partners.pcu.internal.service.IPRService;
import com.gdn.partners.pcu.internal.service.MasterSkuReviewService;
import com.gdn.partners.pcu.internal.service.AutoApprovedServiceWrapper;
import com.gdn.partners.pcu.internal.service.ProductImagePredictionService;
import com.gdn.partners.pcu.internal.service.RecatService;
import com.gdn.partners.pcu.internal.service.SystemParameterService;

import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;

import com.gdn.partners.pcu.internal.service.AssigneeService;
import com.gdn.partners.pcu.internal.service.BrandService;
import com.gdn.partners.pcu.internal.service.BrandWipService;
import com.gdn.partners.pcu.internal.service.BulkProcessService;
import com.gdn.partners.pcu.internal.service.BusinessPartnerService;
import com.gdn.partners.pcu.internal.service.CacheProductService;
import com.gdn.partners.pcu.internal.service.CategoryService;
import com.gdn.partners.pcu.internal.service.DistributionListService;
import com.gdn.partners.pcu.internal.service.ImageService;
import com.gdn.partners.pcu.internal.service.LookupService;
import com.gdn.partners.pcu.internal.service.MTAService;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.PostLiveConfigurationService;
import com.gdn.partners.pcu.internal.service.ProductCenterService;
import com.gdn.partners.pcu.internal.service.ProductMTAWrapper;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.ProductServiceWrapper;
import com.gdn.partners.pcu.internal.service.ProductSuggestionService;
import com.gdn.partners.pcu.internal.service.ProductWorkflowService;
import com.gdn.partners.pcu.internal.service.QCTaskService;
import com.gdn.partners.pcu.internal.service.StoreCopyService;
import com.gdn.partners.pcu.internal.service.UtilityService;
import com.gdn.partners.pcu.internal.service.VendorService;
import com.gdn.partners.pcu.internal.service.IPRService;

/**
 * Created by govind on 20/11/2018 AD.
 */
@Configuration
public class TestConfiguration {

  @Bean
  public ProductService getProductService() {
    return Mockito.mock(ProductService.class);
  }

  @Bean
  public ProductImagePredictionService getProductImagePredictionService() {
    return Mockito.mock(ProductImagePredictionService.class);
  }

  @Bean
  public ProductServiceWrapper getProductServiceWrapper() {
    return Mockito.mock(ProductServiceWrapper.class);
  }

  @Bean
  public BusinessPartnerService getBusinessPartnerService() {
    return Mockito.mock(BusinessPartnerService.class);
  }

  @Bean
  public AssigneeService getAssigneeService() {
    return Mockito.mock(AssigneeService.class);
  }

  @Bean
  public SystemParameterService getSystemParameterService() {
    return Mockito.mock(SystemParameterService.class);
  }

  @Bean
  public ImageService getImageService() {
    return Mockito.mock(ImageService.class);
  }

  @Bean
  public CategoryService getCategoryService() {
    return Mockito.mock(CategoryService.class);
  }

  @Bean
  public CacheProductService getCacheProductService() {
    return Mockito.mock(CacheProductService.class);
  }

  @Bean
  public ProductSuggestionService getExtCatalogService() {
    return Mockito.mock(ProductSuggestionService.class);
  }

  @Bean
  public ProductWorkflowService getProductWorkflowService() {
    return Mockito.mock(ProductWorkflowService.class);
  }

  @Bean
  public RedisTemplate getRedisTemplate() {
    return Mockito.mock(RedisTemplate.class);
  }

  @Bean
  public BulkProcessService getBulkProcessService() {
    return Mockito.mock(BulkProcessService.class);
  }

  @Bean
  public BrandService getBrandService() {
    return Mockito.mock(BrandService.class);
  }

  @Bean
  public BrandWipService getBrandWipService() {
    return Mockito.mock(BrandWipService.class);
  }

  @Bean
  public VendorService getVendorService() {return Mockito.mock(VendorService.class);}

  @Bean
  public ProductMTAWrapper getProductMTAWrapper() {
    return Mockito.mock(ProductMTAWrapper.class);
  }

  @Bean
  public MTAService getMTAService() {
    return Mockito.mock(MTAService.class);
  }

  @Bean
  public PostLiveConfigurationService getPostLiveConfigurationService() {
    return Mockito.mock(PostLiveConfigurationService.class);
  }

  @Bean
  public DistributionListService getDistributionListService() {
    return Mockito.mock(DistributionListService.class);
  }

  @Bean
  public LookupService getLookupService() {
    return Mockito.mock(LookupService.class);
  }

  @Bean
  public MasterSkuReviewService getMasterSkuReviewService() {
    return Mockito.mock(MasterSkuReviewService.class);
  }

  @Bean
  public AutoApprovedService getAutoApprovedService() {
    return Mockito.mock(AutoApprovedService.class);
  }

  @Bean
  public QCTaskService getQCTaskService() {
    return Mockito.mock(QCTaskService.class);
  }

  @Bean
  public PartnersEngineService getPartnersEngineService() {
    return Mockito.mock(PartnersEngineService.class);
  }

  @Bean
  public ProductCenterService getProductCenterService() {
    return Mockito.mock(ProductCenterService.class);
  }

  @Bean
  public RecatService getRecatService() {
    return Mockito.mock(RecatService.class);
  }

  @Bean
  public StoreCopyService storeCopyService() {
    return Mockito.mock(StoreCopyService.class);
  }

  @Bean
  public BulkInternalProcessService bulkInternalProcessService() {
    return Mockito.mock(BulkInternalProcessService.class);
  }

  @Bean
  public BrandAuthorisationService brandAuthorisationService(){
    return Mockito.mock(BrandAuthorisationService.class);
  }

  @Bean
  public UtilityService UtilityService() {
    return Mockito.mock(UtilityService.class);
  }

  @Bean
  public AutoApprovedServiceWrapper ProductAnalyticsService() {
    return Mockito.mock(AutoApprovedServiceWrapper.class);
  }

  @Bean
  public CategoryHistoryService CategoryHistoryService() {
    return Mockito.mock(CategoryHistoryService.class);
  }

  @Bean
  public IPRService IPRService() {
    return Mockito.mock(IPRService.class);
  }

  @Bean
  public FileHelper fileHelper() {
    return Mockito.mock(FileHelper.class);
  }

  @Bean
  public BrandAuthorisationWipService brandAuthorisationWipService() {
    return Mockito.mock(BrandAuthorisationWipService.class);
  }
}
