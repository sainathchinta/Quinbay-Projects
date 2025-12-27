package com.gdn.partners.pcu.external.web.config;

import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.ProductL3Service;
import com.gdn.partners.pcu.external.service.ProductV2Service;
import com.gdn.partners.pcu.external.service.ProductWrapperV2Service;
import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.pcu.external.service.BPService;
import com.gdn.partners.pcu.external.service.BrandService;
import com.gdn.partners.pcu.external.service.BrandWipService;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.CategoryService;
import com.gdn.partners.pcu.external.service.GeneratorService;
import com.gdn.partners.pcu.external.service.ImageService;
import com.gdn.partners.pcu.external.service.MailService;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.ProductBusinessPartnerService;
import com.gdn.partners.pcu.external.service.ProductLevel3WipService;
import com.gdn.partners.pcu.external.service.ProductPricingService;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.QRService;
import com.gdn.partners.pcu.external.service.SellerLogisticsService;

/**
 * Created by govind on 20/11/2018 AD.
 */
@Configuration
public class TestConfiguration {

  @Bean
  public ImageService getCatalogService() {
    return Mockito.mock(ImageService.class);
  }

  @Bean
  public ProductService getProductService() {
    return Mockito.mock(ProductService.class);
  }

  @Bean
  public GeneratorService getGeneratorService() {
    return Mockito.mock(GeneratorService.class);
  }

  @Bean
  public BrandService getBrandService() {
    return Mockito.mock(BrandService.class);
  }

  @Bean
  public BPService getBPService() {
    return Mockito.mock(BPService.class);
  }

  @Bean
  public BusinessPartnerService getBusinessPartnerService() {
    return Mockito.mock(BusinessPartnerService.class);
  }

  @Bean
  public CategoryService getCategoryService() {
    return Mockito.mock(CategoryService.class);
  }

  @Bean
  public PickupPointService getPickupPointService() {
    return Mockito.mock(PickupPointService.class);
  }

  @Bean
  public ProductBusinessPartnerService getProductBusinessPartnerService() {
    return Mockito.mock(ProductBusinessPartnerService.class);
  }

  @Bean
  public BrandWipService getBrandWipService() {
    return Mockito.mock(BrandWipService.class);
  }

  @Bean
  public ProductLevel3WipService getProductLevel3WipService() {
    return Mockito.mock(ProductLevel3WipService.class);
  }

  @Bean
  public QRService getQRService() {
    return Mockito.mock(QRService.class);
  }

  @Bean
  public BulkProcessService getBulkProcessService() { return Mockito.mock(BulkProcessService.class); }

  @Bean
  public MailService getMailService() { return Mockito.mock(MailService.class); }

  @Bean
  public ProductPricingService getPromoMerchantService() { return Mockito.mock(ProductPricingService.class); }

  @Bean
  public SellerLogisticsService getSellerLogisticsService() {
    return Mockito.mock(SellerLogisticsService.class);
  }

  @Bean
  public ProductL3Service getProductL3Service() {
    return Mockito.mock(ProductL3Service.class);
  }

  @Bean
  public ProductV2Service productV2Service() {
    return Mockito.mock(ProductV2Service.class);
  }

  @Bean
  public ProductWrapperV2Service productWrapperV2Service() {
    return Mockito.mock(ProductWrapperV2Service.class);
  }

  @Bean
  public FileStorageService fileStorageService() {
    return Mockito.mock(FileStorageService.class);
  }

}
