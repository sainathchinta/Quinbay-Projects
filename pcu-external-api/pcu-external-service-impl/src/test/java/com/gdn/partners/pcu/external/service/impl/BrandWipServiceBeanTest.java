package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.service.impl.exception.DuplicateEntryException;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class BrandWipServiceBeanTest {

  CreateBrandWipRequest request = new CreateBrandWipRequest();
  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String DEFAULT_BRAND_DESCRIPTION = "Blibli.com";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com.jpg";
  private static final String DEFAULT_BRAND_FOLDER_PATH = "target/test-classes/";
  private static final String DEFAULT_BRAND_FOLDER_NAME = "Brand";
  private static final String FOLDER_PATH_SEPARATOR = "/";
  private static final String JPG_IMAGE_TYPE = "image/jpg";
  private static final String DEFAULT_BRAND_PROFILE_BANNER_PATH = "profile-blibli-com.jpg";
  private static final String BRAND_LOGO_PATH_MORE_THAN_LIMIT =
    "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmno"
      + "pqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
      + "abcdefghijklmnopqrstu.jpg";
  private static final String BRAND_LOGO_PATH_UNDER_LIMIT =
    "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
      + "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
      + "abcdefghijklmnopqrstuvwxyzabcdefghijklmnop.jpg";
  private static final String DEFAULT_REQUEST_ID = "REQUEST_ID";
  private static MockMultipartFile multipartFile;
  private static ClassLoader classLoader;
  private static File file;
  private static int height;
  private static int width;
  private static BufferedImage img;
  private GdnRestSingleResponse<CreateBrandWipResponse> response = new GdnRestSingleResponse<>();
  private UploadImageRequest uploadImageRequest;
  private CreateBrandWipResponse createBrandWipResponse = new CreateBrandWipResponse();
  private GdnRestSingleResponse<BrandResponse> brandResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
  private BrandResponse brandResponse = new BrandResponse();
  private BrandWipResponse brandWipResponse= new BrandWipResponse();
  private GdnRestSingleResponse<BrandWipResponse> brandWipResponseGdnRestSingleResponse;


  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private GCSProperties gcsProperties;

  @Mock
  private FileStorageServiceImpl fileStorageService;

  @InjectMocks
  private BrandWipServiceBean brandWipServiceBean;

  @Captor
  private ArgumentCaptor<CreateBrandWipRequest> createBrandWipRequestArgumentCaptor;

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
    createBrandWipResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    response.setValue(createBrandWipResponse);
    response.setSuccess(true);
    request.setBrandName(DEFAULT_BRAND_NAME);
    request.setBrandDescription(DEFAULT_BRAND_DESCRIPTION);
    request.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    request.setProfileBannerPath(DEFAULT_BRAND_PROFILE_BANNER_PATH);
    uploadImageRequest = new UploadImageRequest();
    uploadImageRequest.setImageFileName(DEFAULT_BRAND_FOLDER_NAME + FOLDER_PATH_SEPARATOR + DEFAULT_BRAND_LOGO_PATH);
    brandResponseGdnRestSingleResponse.setSuccess(true);
    brandResponseGdnRestSingleResponse.setValue(null);
    brandResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandResponse.setBrandDescription(DEFAULT_BRAND_DESCRIPTION);
    brandWipResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandWipResponse.setBrandDescription(DEFAULT_BRAND_DESCRIPTION);
  }

  @AfterEach
  public void teardown() throws Exception {
    verifyNoMoreInteractions(this.pcbFeign);
    verifyNoMoreInteractions(mandatoryParameterHelper);
    verifyNoMoreInteractions(this.systemParameterProperties);
    deleteFolder(DEFAULT_BRAND_FOLDER_PATH + DEFAULT_BRAND_FOLDER_NAME + FOLDER_PATH_SEPARATOR);
    deleteFolder(DEFAULT_BRAND_FOLDER_NAME);
    deleteFolder("null");
  }

  @Test
  public void createTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(
        this.pcbFeign.create(any(CreateBrandWipRequest.class)))
        .thenReturn(response);
    request.setBusinessPartnerName(Constants.BUSINESS_PARTNER_NAME);
    request.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    Mockito.when(this.pcbFeign.findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, DEFAULT_REQUEST_ID));
    MultipartFile multipartFile = this.generateMultipartFile();
    GdnRestSingleResponse<CreateBrandWipResponse> createBrandWipResponseGdnRestSingleResponse =
        this.brandWipServiceBean.create(request, multipartFile, multipartFile);
    Mockito.verify(this.pcbFeign).findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
    Mockito.verify(this.pcbFeign)
        .create(any(CreateBrandWipRequest.class));
    verify(mandatoryParameterHelper,times(2)).getBusinessPartnerCode();
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getValue().getBrandRequestCode());
  }
  @Test
  public void createBrandLogoInGcsTest() throws Exception {
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(
            this.pcbFeign.create(any(CreateBrandWipRequest.class)))
        .thenReturn(response);
    request.setBusinessPartnerName(Constants.BUSINESS_PARTNER_NAME);
    request.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    Mockito.when(this.pcbFeign.findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, DEFAULT_REQUEST_ID));
    MultipartFile multipartFile = this.generateMultipartFile();
    doNothing().when(fileStorageService).createBrandLogoFile(request, multipartFile, response.getValue().getBrandRequestCode());
    GdnRestSingleResponse<CreateBrandWipResponse> createBrandWipResponseGdnRestSingleResponse =
        this.brandWipServiceBean.create(request, multipartFile, multipartFile);
    Mockito.verify(this.pcbFeign).findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
    Mockito.verify(this.pcbFeign)
        .create(any(CreateBrandWipRequest.class));
    verify(mandatoryParameterHelper,times(2)).getBusinessPartnerCode();
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getValue().getBrandRequestCode());
  }

  @Test
  public void createBrandProfileBannerInGcsTest() throws Exception {
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(
            this.pcbFeign.create(any(CreateBrandWipRequest.class)))
        .thenReturn(response);
    request.setBusinessPartnerName(Constants.BUSINESS_PARTNER_NAME);
    request.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    Mockito.when(this.pcbFeign.findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, DEFAULT_REQUEST_ID));
    MultipartFile multipartFile = this.generateMultipartFile();
    doNothing().when(fileStorageService).createBrandProfileBannerFile(request, multipartFile, response.getValue().getBrandRequestCode());
    GdnRestSingleResponse<CreateBrandWipResponse> createBrandWipResponseGdnRestSingleResponse =
        this.brandWipServiceBean.create(request, multipartFile, multipartFile);
    Mockito.verify(this.pcbFeign).findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
    Mockito.verify(this.pcbFeign)
        .create(any(CreateBrandWipRequest.class));
    verify(mandatoryParameterHelper,times(2)).getBusinessPartnerCode();
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getValue().getBrandRequestCode());
  }

  @Test
  public void createBrandWithLargeBrandNameTest() throws Exception {
    String brandName =
      BRAND_LOGO_PATH_MORE_THAN_LIMIT.substring(0, BRAND_LOGO_PATH_MORE_THAN_LIMIT.lastIndexOf(Constants.DOT_SEPARATOR));
    request.setBrandLogoPath(BRAND_LOGO_PATH_MORE_THAN_LIMIT);
    request.setBusinessPartnerName(Constants.BUSINESS_PARTNER_NAME);
    request.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    request.setBrandName(brandName);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(
        this.pcbFeign.create(any(CreateBrandWipRequest.class)))
      .thenReturn(response);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    Mockito.when(this.pcbFeign.findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(new GdnRestSingleResponse<>(null, DEFAULT_REQUEST_ID));
    MultipartFile multipartFile = this.generateMultipartLargeFileName();
    GdnRestSingleResponse<CreateBrandWipResponse> createBrandWipResponseGdnRestSingleResponse =
      this.brandWipServiceBean.create(request, multipartFile, multipartFile);
    Mockito.verify(this.pcbFeign).findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.pcbFeign).filterByBrandName(brandName, true);
    Mockito.verify(this.pcbFeign).create(createBrandWipRequestArgumentCaptor.capture());
    verify(mandatoryParameterHelper,times(2)).getBusinessPartnerCode();
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getValue().getBrandRequestCode());
    Assertions.assertEquals(BRAND_LOGO_PATH_UNDER_LIMIT, createBrandWipRequestArgumentCaptor.getValue().getBrandLogoPath());
    Assertions.assertEquals(brandName, createBrandWipRequestArgumentCaptor.getValue().getBrandName());
  }

  @Test
  public void createInternalBrandCreationTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(StringUtils.EMPTY);
    Mockito.when(
        this.pcbFeign.create(any(CreateBrandWipRequest.class)))
        .thenReturn(response);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    Mockito.when(this.pcbFeign.findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, DEFAULT_REQUEST_ID));

    request.setBusinessPartnerCode(StringUtils.EMPTY);
    request.setBusinessPartnerName(StringUtils.EMPTY);
    MultipartFile multipartFile = this.generateMultipartFile();
    GdnRestSingleResponse<CreateBrandWipResponse> createBrandWipResponseGdnRestSingleResponse =
        this.brandWipServiceBean.create(request, multipartFile, multipartFile);
    Mockito.verify(this.pcbFeign).findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
    Mockito.verify(this.pcbFeign)
        .create(any(CreateBrandWipRequest.class));
    verify(mandatoryParameterHelper,times(2)).getBusinessPartnerCode();
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getValue().getBrandRequestCode());
  }

  @Test
  public void create_existingBrandTest() throws Exception {
    brandResponseGdnRestSingleResponse.setValue(brandResponse);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    try{
      this.brandWipServiceBean.create(request, multipartFile, multipartFile);
    } catch(DuplicateEntryException e){
    } finally {
      Mockito.verify(this.pcbFeign).filterByBrandName(Mockito.anyString(), Mockito.eq(true));
    }
  }

  @Test
  public void create_existingBrandWipTest() throws Exception {
    brandWipResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(brandWipResponse, DEFAULT_BRAND_DESCRIPTION);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    Mockito.when(this.pcbFeign.findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(brandWipResponseGdnRestSingleResponse);
    try{
      this.brandWipServiceBean.create(request, multipartFile, multipartFile);
    } catch(DuplicateEntryException e){
    } finally {
      Mockito.verify(this.pcbFeign).findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
      verify(mandatoryParameterHelper,times(2)).getBusinessPartnerCode();
    }
  }

  @Test
  public void createTest_noBrandLogoProfileBanner() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(
        this.pcbFeign.create(any(CreateBrandWipRequest.class)))
        .thenReturn(response);
    Mockito.when(this.pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.eq(true))).thenReturn(brandResponseGdnRestSingleResponse);
    Mockito.when(this.pcbFeign.findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, DEFAULT_REQUEST_ID));
    GdnRestSingleResponse<CreateBrandWipResponse> createBrandWipResponseGdnRestSingleResponse =
        this.brandWipServiceBean.create(request, null, null);
    Mockito.verify(this.pcbFeign).findByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
    Mockito.verify(this.pcbFeign)
        .create(any(CreateBrandWipRequest.class));
    verify(mandatoryParameterHelper,times(2)).getBusinessPartnerCode();
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getValue().getBrandRequestCode());
  }

  @Test
  public void findByNameTest() throws Exception {
    BrandResponse brandResponse1 = new BrandResponse();
    brandResponse1.setBrandName(DEFAULT_BRAND_NAME);
    GdnRestSingleResponse<BrandResponse> responseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    responseGdnRestSingleResponse.setValue(brandResponse1);
    responseGdnRestSingleResponse.setSuccess(true);
    Mockito.when(this.pcbFeign.filterByBrandName(DEFAULT_BRAND_NAME, true)).thenReturn(responseGdnRestSingleResponse);
    BrandResponse brandResponse = this.brandWipServiceBean.findByName(DEFAULT_BRAND_NAME);
    Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
    Assertions.assertEquals(DEFAULT_BRAND_NAME, brandResponse.getBrandName());
    Assertions.assertEquals(DEFAULT_BRAND_NAME, responseGdnRestSingleResponse.getValue().getBrandName());
  }

  private MultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile(DEFAULT_BRAND_FOLDER_PATH + DEFAULT_BRAND_FOLDER_NAME + FOLDER_PATH_SEPARATOR + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource(
      DEFAULT_BRAND_FOLDER_NAME + FOLDER_PATH_SEPARATOR + BrandWipServiceBeanTest.DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile(BrandWipServiceBeanTest.DEFAULT_BRAND_LOGO_PATH,
        BrandWipServiceBeanTest.DEFAULT_BRAND_LOGO_PATH, JPG_IMAGE_TYPE, IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }

  private MultipartFile generateMultipartLargeFileName() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile(
      DEFAULT_BRAND_FOLDER_PATH + DEFAULT_BRAND_FOLDER_NAME + FOLDER_PATH_SEPARATOR + BRAND_LOGO_PATH_UNDER_LIMIT);
    file = new File(
      classLoader.getResource(DEFAULT_BRAND_FOLDER_NAME + FOLDER_PATH_SEPARATOR + BRAND_LOGO_PATH_UNDER_LIMIT)
        .getFile());
    multipartFile = new MockMultipartFile(DEFAULT_BRAND_LOGO_PATH, BRAND_LOGO_PATH_UNDER_LIMIT, JPG_IMAGE_TYPE,
      IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }

  private void mockFile(String filePath) throws IOException {
    file = new File(filePath);
    file.mkdirs();
    width = 640;
    height = 320;
    img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }
}
