package com.gdn.partners.pcu.external.service.impl;

import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerAttributeServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductItemBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.web.model.request.DefaultConfigurationAndPickupPointRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSettingsRequest;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.GeolocationDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProductSettingDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import org.springframework.data.domain.PageImpl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Created by govind on 21/12/2018 AD.
 */
public class ProductBusinessPartnerServiceImplTest {

  private static final String PROFILE_ID = "profile_id";
  private static final String GDN_PRODUCT_ITEM_SKU = "gdn-product-item-sku";
  private static final String GDN_PRODUCT_SKU = "gdn-product-sku";
  private static final String ATTRIBUTE_ID = "attribute-id";
  private static final String ATTR_VALUE = "attr-value";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICK_UP_POINT_NAME = "pickupPoint1";
  private static final String PICK_UP_POINT_CODE = "pickupPointCode";
  private static final String SOURCE_BUSINESS_PARTNER_CODE = "sourceBusinessPartnerCode";
  private static final String UNAUTHORIZE_ERROR = "Unauthorize access :You are not authorized";
  private static final String STORE_ID = "store-id";
  private List<PickupPointOutboundResponse> pickupPointResponseList;

  @InjectMocks
  private ProductBusinessPartnerServiceImpl productBusinessPartnerService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private PBPFeign pbpFeign;

  private GdnBaseRestResponse gdnBaseRestResponse;

  private ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequest;

  private ProfileResponse profileResponse;

  private PickupPointDTO pickupPointDTO;

  private DefaultConfigurationAndPickupPointRequest defaultConfigurationAndPickupPointRequest;

  private PickupPointOutboundResponse pickupPointResponse;

  @Captor
  private ArgumentCaptor<ProductBusinessPartnerRequest> productBusinessPartnerRequest;

  @Captor
  private ArgumentCaptor<ProductCopyRequest> productCopyRequestCaptor;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(Boolean.TRUE);
    profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    profileResponse.setCompany(company);
    profileResponse.setId(PROFILE_ID);
    profileResponse.setActivated(true);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_LIMIT, true);
    profileResponse.setFlags(flags);
    List<PickupPointDTO> pickupPoints = new ArrayList<>();
    PickupPointDTO pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setActivated(true);
    pickupPointDTO.setName(PICK_UP_POINT_NAME);
    pickupPointDTO.setCode(PICK_UP_POINT_CODE);
    pickupPoints.add(pickupPointDTO);
    profileResponse.setPickupPoints(pickupPoints);

    pickupPointResponse = new PickupPointOutboundResponse();
    BeanUtils.copyProperties(pickupPointDTO, pickupPointResponse);
    pickupPointResponse.setGeolocation(new GeolocationDTO());

    productBusinessPartnerServiceRequest = new ProductBusinessPartnerServiceRequest();
    productBusinessPartnerServiceRequest.setBusinessPartnerCode(PROFILE_ID);
    productBusinessPartnerServiceRequest.setStoreId(Constants.STORE_ID);
    ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerServiceRequest =
        new ProductItemBusinessPartnerServiceRequest();
    productItemBusinessPartnerServiceRequest.setGdnProductItemSku(GDN_PRODUCT_ITEM_SKU);
    List<ProductItemBusinessPartnerServiceRequest> productItemBusinessPartnerServiceRequests = new ArrayList<>();
    productItemBusinessPartnerServiceRequests.add(productItemBusinessPartnerServiceRequest);
    productBusinessPartnerServiceRequest.setProductItemBusinessPartners(productItemBusinessPartnerServiceRequests);

    ProductBusinessPartnerAttributeServiceRequest productBusinessPartnerAttributeServiceRequest =
        new ProductBusinessPartnerAttributeServiceRequest();
    productBusinessPartnerAttributeServiceRequest.setAttributeId(ATTRIBUTE_ID);
    productBusinessPartnerAttributeServiceRequest.setValue(ATTR_VALUE);
    List<ProductBusinessPartnerAttributeServiceRequest>
        productBusinessPartnerAttributeServiceRequests = new ArrayList<>();
    productBusinessPartnerAttributeServiceRequests.add(productBusinessPartnerAttributeServiceRequest);
    productBusinessPartnerServiceRequest.setProductBusinessPartnerAttributes(productBusinessPartnerAttributeServiceRequests);
    defaultConfigurationAndPickupPointRequest = new DefaultConfigurationAndPickupPointRequest();

    pickupPointResponseList = Arrays.asList(pickupPointResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(businessPartnerService);
    verifyNoMoreInteractions(pbpFeign);
    verifyNoMoreInteractions(systemParameterProperties);
    verifyNoMoreInteractions(pickupPointService);

  }

  @Test
  public void createTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(PROFILE_ID))
          .thenReturn(profileResponse);
      Mockito.when(this.pbpFeign.create(any(ProductBusinessPartnerRequest.class)))
          .thenReturn(gdnBaseRestResponse);
      this.productBusinessPartnerService.create(productBusinessPartnerServiceRequest);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(PROFILE_ID);
      Mockito.verify(pickupPointService)
          .validateAndSaveDefaultPickupPoint(productBusinessPartnerServiceRequest);
      Mockito.verify(this.pbpFeign).create(productBusinessPartnerRequest.capture());
      ProductBusinessPartnerRequest webRequest = productBusinessPartnerRequest.getValue();
      Assertions.assertEquals(PROFILE_ID, webRequest.getBusinessPartnerId());
      Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
          webRequest.getProductItemBusinessPartners().get(0).getGdnProductItemSku());
      Assertions.assertEquals(ATTRIBUTE_ID,
          webRequest.getProductBusinessPartnerAttributes().get(0).getAttributeId());
      Assertions.assertEquals(ATTR_VALUE,
          webRequest.getProductBusinessPartnerAttributes().get(0).getValue());
      Assertions.assertEquals(Constants.STORE_ID, webRequest.getStoreId());
      Assertions.assertEquals(Constants.STORE_ID,
          webRequest.getProductItemBusinessPartners().get(0).getStoreId());
      Assertions.assertEquals(Constants.STORE_ID,
          webRequest.getProductBusinessPartnerAttributes().get(0).getStoreId());
    }
  }

  @Test
  public void createTest_whenLogisticsNotUpdated() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(PROFILE_ID))
          .thenReturn(profileResponse);
      Mockito.when(this.pbpFeign.create(any(ProductBusinessPartnerRequest.class)))
          .thenReturn(response);
      this.productBusinessPartnerService.create(productBusinessPartnerServiceRequest);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(PROFILE_ID);
      Mockito.verify(pickupPointService)
          .validateAndSaveDefaultPickupPoint(productBusinessPartnerServiceRequest);
      Mockito.verify(this.pbpFeign).create(productBusinessPartnerRequest.capture());
      ProductBusinessPartnerRequest webRequest = productBusinessPartnerRequest.getValue();
      Assertions.assertEquals(PROFILE_ID, webRequest.getBusinessPartnerId());
      Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
          webRequest.getProductItemBusinessPartners().get(0).getGdnProductItemSku());
      Assertions.assertEquals(ATTRIBUTE_ID,
          webRequest.getProductBusinessPartnerAttributes().get(0).getAttributeId());
      Assertions.assertEquals(ATTR_VALUE,
          webRequest.getProductBusinessPartnerAttributes().get(0).getValue());
      Assertions.assertEquals(Constants.STORE_ID, webRequest.getStoreId());
      Assertions.assertEquals(Constants.STORE_ID,
          webRequest.getProductItemBusinessPartners().get(0).getStoreId());
      Assertions.assertEquals(Constants.STORE_ID,
          webRequest.getProductBusinessPartnerAttributes().get(0).getStoreId());
    }
  }

  @Test
  public void create_WhenProductTypeBigProductTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      this.profileResponse.getCompany().setInternationalFlag(true);
      pickupPointResponse.setOriginId("Origin-Jakarta");
      pickupPointResponse.setCode("PP123");
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(PROFILE_ID))
          .thenReturn(profileResponse);
      Mockito.when(businessPartnerService.getPickupPointByCode("PP123"))
          .thenReturn(pickupPointResponse);
      Mockito.when(this.pbpFeign.create(any(ProductBusinessPartnerRequest.class)))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(systemParameterProperties.getDefaultMerchantInternationalOriginId())
          .thenReturn("Origin-Jakarta");
      ProductBusinessPartnerServiceRequest request = new ProductBusinessPartnerServiceRequest();
      request.setBusinessPartnerCode(PROFILE_ID);
      List<ProductItemBusinessPartnerServiceRequest> productItemBusinessPartnerRequests =
          new ArrayList<>();
      ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerRequest =
          new ProductItemBusinessPartnerServiceRequest();
      productItemBusinessPartnerRequest.setPickupPointId("PP123");
      productItemBusinessPartnerRequests.add(productItemBusinessPartnerRequest);
      request.setProductItemBusinessPartners(productItemBusinessPartnerRequests);
      this.productBusinessPartnerService.create(request);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(PROFILE_ID);
      Mockito.verify(businessPartnerService).getPickupPointByCode("PP123");
      Mockito.verify(systemParameterProperties).getDefaultMerchantInternationalOriginId();

      Mockito.verify(pickupPointService).validateAndSaveDefaultPickupPoint(request);
      Mockito.verify(this.pbpFeign).create(productBusinessPartnerRequest.capture());
      ProductBusinessPartnerRequest webRequest = productBusinessPartnerRequest.getValue();
      Assertions.assertEquals(PROFILE_ID, webRequest.getBusinessPartnerId());
    }
  }

  @Test
  public void getPickUpPointsTest() {
    ProductSettingDTO productSettingDTO = new ProductSettingDTO();
    productSettingDTO.setCnc(true);
    profileResponse.setProductSettings(productSettingDTO);
    profileResponse.setMultiDefaultAddressFlag(true);
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_LIMIT, true);
    profileResponse.setFlags(flags);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESS_PARTNER_CODE))
      .thenReturn(pickupPointResponseList);
    BusinessPartnerProfileWebResponse response =
        productBusinessPartnerService.getBusinessPartnerProfile(BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(PICK_UP_POINT_NAME, response.getPickupPoints().get(0).getName());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertTrue(response.getProductSettings().getCnc());
    Assertions.assertTrue(response.getMultiDefaultAddressFlag());
    Assertions.assertTrue(response.isProductLimitEnabled());
  }

  @Test
  public void create_whenClientExceptionTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(PROFILE_ID))
          .thenReturn(profileResponse);
      Mockito.when(this.pbpFeign.create(any(ProductBusinessPartnerRequest.class))).thenReturn(null);
      ProductBusinessPartnerServiceRequest request = new ProductBusinessPartnerServiceRequest();
      request.setBusinessPartnerCode(PROFILE_ID);
      try {
        this.productBusinessPartnerService.create(request);
      } catch (ClientException ex) {
      } finally {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(PROFILE_ID);
        Mockito.verify(pickupPointService).validateAndSaveDefaultPickupPoint(request);
        Mockito.verify(this.pbpFeign).create(productBusinessPartnerRequest.capture());
        ProductBusinessPartnerRequest webRequest = productBusinessPartnerRequest.getValue();
        Assertions.assertEquals(PROFILE_ID, webRequest.getBusinessPartnerId());

      }
    }
  }

  @Test
  public void create_withInactiveMerchantExceptionTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      profileResponse.setMerchantStatus(null);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(PROFILE_ID))
          .thenReturn(profileResponse);
      ProductBusinessPartnerServiceRequest request = new ProductBusinessPartnerServiceRequest();
      request.setBusinessPartnerCode(PROFILE_ID);
      try {
        this.productBusinessPartnerService.create(request);
      } catch (ApplicationRuntimeException ex) {
      } finally {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(PROFILE_ID);
      }
    }
  }

  @Test
  public void create_ExceptionUnauthorizeErrorTest() throws Exception {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      ProductBusinessPartnerServiceRequest request = new ProductBusinessPartnerServiceRequest();
      try {
        this.productBusinessPartnerService.create(request);
      } catch (ApplicationRuntimeException e) {
        errorMessage = e.getErrorMessage();
      } finally {
        Assertions.assertEquals(UNAUTHORIZE_ERROR, errorMessage);
      }
    }
  }

  @Test
  public void productItemCopyTest() {
    Mockito
      .when(this.pbpFeign.copy(Mockito.eq(false), any(ProductCopyRequest.class)))
      .thenReturn(gdnBaseRestResponse);

    ProductCopyRequest productCopyRequest = new ProductCopyRequest();
    productCopyRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCopyRequest.setSourceBusinessPartnerCode(SOURCE_BUSINESS_PARTNER_CODE);
    productCopyRequest.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU, Arrays.asList(GDN_PRODUCT_ITEM_SKU)));

    Mockito.when(businessPartnerService.filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class)))
      .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse)));

    this.productBusinessPartnerService.copy(productCopyRequest, false);

    Mockito.verify(businessPartnerService).filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class));

    Mockito.verify(this.pbpFeign).copy(Mockito.eq(false), productCopyRequestCaptor.capture());

    Assertions.assertEquals(PICK_UP_POINT_CODE, productCopyRequest.getPickupPointCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productCopyRequest.getBusinessPartnerCode());
    Assertions.assertEquals(SOURCE_BUSINESS_PARTNER_CODE, productCopyRequest.getSourceBusinessPartnerCode());
    Assertions.assertTrue(productCopyRequest.getGdnItemSkus().size() == 1);
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU, productCopyRequest.getGdnItemSkus().get(GDN_PRODUCT_SKU).get(0));
  }

  @Test
  public void productItemCopyTest_whenNoPickUpPointFound() {
    Mockito
      .when(this.pbpFeign.copy(Mockito.eq(false), any(ProductCopyRequest.class)))
      .thenReturn(gdnBaseRestResponse);

    Mockito.when(businessPartnerService.filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class)))
      .thenReturn(new PageImpl<>(new ArrayList<>()));

    ProductCopyRequest productCopyRequest = new ProductCopyRequest();
    productCopyRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCopyRequest.setSourceBusinessPartnerCode(SOURCE_BUSINESS_PARTNER_CODE);
    productCopyRequest.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU, Arrays.asList(GDN_PRODUCT_ITEM_SKU)));

    try {
      this.productBusinessPartnerService.copy(productCopyRequest, false);
    } catch(ApplicationRuntimeException e){
      Mockito.verify(businessPartnerService).filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class));
    }
  }

  @Test
  public void productItemCopyTest_withRetryFlag() {
    Mockito
      .when(this.pbpFeign.copy(Mockito.eq(true), any(ProductCopyRequest.class)))
      .thenReturn(gdnBaseRestResponse);

    ProductCopyRequest productCopyRequest = new ProductCopyRequest();
    productCopyRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCopyRequest.setSourceBusinessPartnerCode(SOURCE_BUSINESS_PARTNER_CODE);
    productCopyRequest.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU, Arrays.asList(GDN_PRODUCT_ITEM_SKU)));

    Mockito.when(businessPartnerService.filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class)))
      .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse)));

    this.productBusinessPartnerService.copy(productCopyRequest, true);

    Mockito.verify(businessPartnerService).filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class));

    Mockito.verify(this.pbpFeign).copy(Mockito.eq(true), productCopyRequestCaptor.capture());

    Assertions.assertEquals(PICK_UP_POINT_CODE, productCopyRequest.getPickupPointCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productCopyRequest.getBusinessPartnerCode());
    Assertions.assertEquals(SOURCE_BUSINESS_PARTNER_CODE, productCopyRequest.getSourceBusinessPartnerCode());
    Assertions.assertTrue(productCopyRequest.getGdnItemSkus().size() == 1);
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU, productCopyRequest.getGdnItemSkus().get(GDN_PRODUCT_SKU).get(0));
  }

  @Test
  public void productItemCopyTest_whenFailToCopy() {
   this.gdnBaseRestResponse.setSuccess(false);
    Mockito
      .when(this.pbpFeign.copy(Mockito.eq(false), any(ProductCopyRequest.class)))
      .thenReturn(gdnBaseRestResponse);

    ProductCopyRequest productCopyRequest = new ProductCopyRequest();
    productCopyRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    Mockito.when(businessPartnerService.filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse)));

    try {
      this.productBusinessPartnerService.copy(productCopyRequest, false);
    } catch(ClientException e){
      Mockito.verify(businessPartnerService).filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class));
      Mockito.verify(this.pbpFeign).copy(Mockito.eq(false), productCopyRequestCaptor.capture());
    }
  }

  @Test
  public void productAllItemsCopyTest() {
    Mockito
      .when(this.pbpFeign.copyAll(any(ProductCopyRequest.class)))
      .thenReturn(gdnBaseRestResponse);

    ProductCopyRequest productCopyRequest = new ProductCopyRequest();
    productCopyRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCopyRequest.setSourceBusinessPartnerCode(SOURCE_BUSINESS_PARTNER_CODE);
    productCopyRequest.setGdnItemSkus(Collections.emptyMap());

    Mockito.when(businessPartnerService.filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse)));

    this.productBusinessPartnerService.copyAll(productCopyRequest);

    Mockito.verify(businessPartnerService).filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), any(PickupPointFilterRequest.class));

    Mockito.verify(this.pbpFeign).copyAll(productCopyRequestCaptor.capture());

    Assertions.assertEquals(PICK_UP_POINT_CODE, productCopyRequest.getPickupPointCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productCopyRequest.getBusinessPartnerCode());
    Assertions.assertEquals(SOURCE_BUSINESS_PARTNER_CODE, productCopyRequest.getSourceBusinessPartnerCode());
    Assertions.assertTrue(productCopyRequest.getGdnItemSkus().isEmpty());
  }

  @Test
  public void productsAvailableToDirectCopyTest() {
    Mockito.when(this.pbpFeign
      .productsAvailableToCopy(eq(BUSINESS_PARTNER_CODE), eq(SOURCE_BUSINESS_PARTNER_CODE), eq(0), eq(10),
        any(ProductLevel3SummaryRequest.class)))
      .thenReturn(new GdnRestListResponse<>(null, null, true, null));

    this.productBusinessPartnerService
      .productsAvailableToDirectCopy(BUSINESS_PARTNER_CODE, SOURCE_BUSINESS_PARTNER_CODE, 0, 10,
        new ProductLevel3SummaryRequest());

    Mockito.verify(this.pbpFeign)
      .productsAvailableToCopy(eq(BUSINESS_PARTNER_CODE), eq(SOURCE_BUSINESS_PARTNER_CODE), eq(0), eq(10),
        any(ProductLevel3SummaryRequest.class));
  }

  @Test
  public void productsAvailableToDirectCopyTest_whenInvalidClientResponse() {
    Mockito.when(this.pbpFeign
      .productsAvailableToCopy(eq(BUSINESS_PARTNER_CODE), eq(SOURCE_BUSINESS_PARTNER_CODE), eq(0), eq(10),
        any(ProductLevel3SummaryRequest.class)))
      .thenReturn(new GdnRestListResponse<>(null, null, false, null));

    try {
      this.productBusinessPartnerService
        .productsAvailableToDirectCopy(BUSINESS_PARTNER_CODE, SOURCE_BUSINESS_PARTNER_CODE, 0, 10,
          new ProductLevel3SummaryRequest());
    } catch(ClientException e) {
      Mockito.verify(this.pbpFeign)
        .productsAvailableToCopy(eq(BUSINESS_PARTNER_CODE), eq(SOURCE_BUSINESS_PARTNER_CODE), eq(0), eq(10),
          any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void productsAvailableToDirectCopyTest_whenNullClientResponse() {
    Mockito.when(this.pbpFeign
      .productsAvailableToCopy(eq(BUSINESS_PARTNER_CODE), eq(SOURCE_BUSINESS_PARTNER_CODE), eq(0), eq(10),
        any(ProductLevel3SummaryRequest.class)))
      .thenReturn(null);

    try {
      this.productBusinessPartnerService
        .productsAvailableToDirectCopy(BUSINESS_PARTNER_CODE, SOURCE_BUSINESS_PARTNER_CODE, 0, 10,
          new ProductLevel3SummaryRequest());
    } catch(ClientException e) {
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, e.getMessage());
    } finally {
      Mockito.verify(this.pbpFeign)
        .productsAvailableToCopy(eq(BUSINESS_PARTNER_CODE), eq(SOURCE_BUSINESS_PARTNER_CODE), eq(0), eq(10),
          any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void isProductMappedToMerchant() {
    Mockito.when(this.pbpFeign.isProductMappedToMerchant(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSimpleResponse<>(null, true));
    boolean productMappedToMerchant =
        this.productBusinessPartnerService.isProductMappedToMerchant(BUSINESS_PARTNER_CODE);
    Mockito.verify(pbpFeign).isProductMappedToMerchant(BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(productMappedToMerchant);
  }

  @Test
  public void updateDefaultConfigurationsTest() {
    defaultConfigurationAndPickupPointRequest.setDefaultConfigurationUpdated(true);
    defaultConfigurationAndPickupPointRequest.setProductSettings(new ProductSettingsRequest());
    this.productBusinessPartnerService.updateDefaultConfigurationsAndPickupPoints(STORE_ID,
        BUSINESS_PARTNER_CODE, defaultConfigurationAndPickupPointRequest);
    Mockito.verify(businessPartnerService).updateDefaultConfiguration(any());
  }

  @Test
  public void updateDefaultPickupPointsTest() {
    defaultConfigurationAndPickupPointRequest.setPickupCodesUpdated(true);
    this.productBusinessPartnerService.updateDefaultConfigurationsAndPickupPoints(STORE_ID,
        BUSINESS_PARTNER_CODE, defaultConfigurationAndPickupPointRequest);
    Mockito.verify(businessPartnerService).updateDefaultPickupPointCode(any());
  }

  @Test
  public void testGetBusinessPartnerProfile_WithNullBusinessPartnerCodeException() {
    String businessPartnerCode = null;
    try {
      productBusinessPartnerService.getBusinessPartnerProfile(businessPartnerCode);
    } catch (ValidationException ex) {
      assertEquals(ErrorCategory.REQUIRED_PARAMETER.getMessage() + ErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY,
          ex.getMessage());
    }
  }
}
