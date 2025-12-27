package com.gdn.partners.pcu.internal.service.impl;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandApprovalResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandRejectionWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BrandWipServiceImplTest {

  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String DEFAULT_BRAND_DESCRIPTION = "Blibli.com";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";
  private static final String DEFAULT_BRAND_CODE = "BR-0001";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.";
  private static final String DEFAULT_PROFILE_BANNER_PATH = "blibli-com-banner.";
  private static final String DRAFT = "DRAFT";
  private static final String ID = "ID";
  private static final int PAGE = 0;
  private static final int SIZE = 10;

  private BrandWipResponse brandWipResponse = new BrandWipResponse();
  private GdnRestSingleResponse<BrandWipResponse> response = new GdnRestSingleResponse<>();
  private GdnRestSingleResponse<BrandRejectionInfoResponse> brandRejectionResponse;
  private List<BrandWipResponse> brandWipResponseList = new ArrayList<>();
  private GdnRestListResponse<BrandWipResponse> brandWipResponseGdnRestListResponse = new GdnRestListResponse<>();
  private BrandWipSummaryRequest brandWipSummaryRequest = new BrandWipSummaryRequest();
  private GdnRestListResponse<BrandWipHistoryResponse> historyResponse = new GdnRestListResponse<>();
  private List<BrandWipHistoryResponse> historyList = new ArrayList<>();
  private BrandWipHistoryResponse brandWipHistoryResponse = new BrandWipHistoryResponse();
  private BrandWipHistorySummaryRequest brandWipHistorySummaryRequest = new BrandWipHistorySummaryRequest();
  private ApproveBrandWipWebRequest approveBrandWipWebRequest;
  private BrandApproveRequest brandApproveRequest;
  private BrandRejectRequest brandRejectRequest;
  private BrandWipResponse brandWipResponse1;
  private GdnRestSingleResponse<CreateBrandResponse> createBrandResponseGdnRestSingleResponse;

  private static MockMultipartFile multipartFile;
  private static ClassLoader classLoader;
  private static File file;
  private static int height;
  private static int width;
  private static BufferedImage img;

  @InjectMocks
  BrandWipServiceImpl brandWipService;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private FileStorageService fileStorageService;

  @Captor
  private ArgumentCaptor<BrandApproveRequest> brandApproveRequestArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    approveBrandWipWebRequest = new ApproveBrandWipWebRequest();
    approveBrandWipWebRequest.setBrandName(DEFAULT_BRAND_NAME);
    approveBrandWipWebRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    approveBrandWipWebRequest.setBrandDescription(DEFAULT_BRAND_DESCRIPTION);

    brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandApproveRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandApproveRequest.setBrandDescription(DEFAULT_BRAND_DESCRIPTION);
    brandApproveRequest.setBrandLogoPath(null);
    brandApproveRequest.setProfileBannerPath(null);

    brandWipResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandWipResponse.setBrandDescription(DEFAULT_BRAND_DESCRIPTION);
    response.setSuccess(true);
    response.setValue(brandWipResponse);
    brandWipSummaryRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandWipSummaryRequest.setState(DRAFT);
    brandWipResponseList.add(brandWipResponse);
    brandWipResponseGdnRestListResponse.setContent(brandWipResponseList);
    brandWipResponseGdnRestListResponse.setSuccess(true);

    brandWipHistoryResponse.setState(BrandWipState.DRAFT.getDescription());
    brandWipHistoryResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setDescription(DEFAULT_BRAND_DESCRIPTION);
    historyList.add(brandWipHistoryResponse);
    historyResponse.setSuccess(true);
    historyResponse.setContent(historyList);
    brandWipHistorySummaryRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);

    brandRejectRequest = new BrandRejectRequest();
    brandRejectRequest.setBrandRequestCode(DEFAULT_BRAND_CODE);

    brandWipResponse1 = new BrandWipResponse();
    brandWipResponse1.setBrandCode(DEFAULT_BRAND_CODE);
    BrandRejectionInfoResponse brandRejectionInfoResponse = new BrandRejectionInfoResponse();
    brandRejectionInfoResponse.setId(ID);
    brandRejectionInfoResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandRejectionInfoResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandRejectionInfoResponse.setRejectionReason(DEFAULT_BRAND_DESCRIPTION);
    brandRejectionResponse = new GdnRestSingleResponse<>(brandRejectionInfoResponse, Constants.REQUEST_ID);
    createBrandResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(
        new CreateBrandResponse(DEFAULT_BRAND_CODE, DEFAULT_BRAND_LOGO_PATH, DEFAULT_PROFILE_BANNER_PATH),
        DEFAULT_REQUEST_ID);

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign, systemParameterProperties);
    deleteFolder("null/");
    deleteFolder(DEFAULT_BRAND_LOGO_PATH);
    deleteFolder(DEFAULT_PROFILE_BANNER_PATH);
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  public void approveBrandTest() throws Exception {
    approveBrandWipWebRequest.setValidBrand(true);
    approveBrandWipWebRequest.setProtectedBrand(true);
    Mockito.when(this.systemParameterProperties.getDirectoryProfileBannerSource())
        .thenReturn(DEFAULT_PROFILE_BANNER_PATH);
    Mockito.when(this.systemParameterProperties.getDirectoryProfileBannerFinal())
        .thenReturn(DEFAULT_PROFILE_BANNER_PATH);
    Mockito.when(pcbFeign.approveBrand(Mockito.anyString(), Mockito.any()))
        .thenReturn(createBrandResponseGdnRestSingleResponse);
    MultipartFile multipartFile = generateMultipartFile();
    BrandApprovalResponse response =
        brandWipService.approveBrand(approveBrandWipWebRequest, multipartFile, multipartFile);
    Mockito.verify(pcbFeign).approveBrand(Mockito.eq(approveBrandWipWebRequest.getBrandRequestCode()),
        brandApproveRequestArgumentCaptor.capture());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, response.getBrandCode());
    Assertions.assertTrue(brandApproveRequestArgumentCaptor.getValue().isValidBrand());
    Assertions.assertTrue(brandApproveRequestArgumentCaptor.getValue().isProtectedBrand());
  }

  @Test
  public void approveBrandExceptionTest() throws Exception {
    Mockito.when(pcbFeign.approveBrand(Mockito.anyString(), Mockito.any())).thenReturn(null);
    try {
      MultipartFile multipartFile = generateMultipartFile();
      brandWipService.approveBrand(approveBrandWipWebRequest, multipartFile, multipartFile);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(pcbFeign).approveBrand(Mockito.anyString(), Mockito.any());
    }
  }

  private MultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource("Brand/" + DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile(DEFAULT_BRAND_LOGO_PATH, DEFAULT_BRAND_LOGO_PATH, "image/jpg",
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
  }

  @Test
  public void getBrandWipDetailTest() {
    Mockito.when(this.pcbFeign.getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE)).thenReturn(response);
    GdnRestSingleResponse<BrandWipResponse> restSingleResponse =
        this.brandWipService.getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.pcbFeign).getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE);
    Assertions.assertEquals(DEFAULT_BRAND_NAME, restSingleResponse.getValue().getBrandName());
    Assertions.assertEquals(DEFAULT_BRAND_DESCRIPTION, restSingleResponse.getValue().getBrandDescription());
  }

  @Test
  public void getBrandRejectedReasonTest() {
    Mockito.when(this.pcbFeign.getBrandRejectionReasonByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandRejectionResponse);
    BrandRejectionWebResponse brandRejectionInfoResponse =
        this.brandWipService.getBrandRejectionReasonByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.pcbFeign).getBrandRejectionReasonByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    Assertions.assertEquals(DEFAULT_BRAND_NAME, brandRejectionInfoResponse.getBrandName());
    Assertions.assertEquals(DEFAULT_BRAND_DESCRIPTION, brandRejectionInfoResponse.getRejectionReason());
  }

  @Test
  public void getBrandWipDetail_expectException() {
    Mockito.when(this.pcbFeign.getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE)).thenReturn(null);
    try {
      GdnRestSingleResponse<BrandWipResponse> restSingleResponse = this.brandWipService.getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE);
    } catch (ClientException e){
    } finally {
      Mockito.verify(this.pcbFeign).getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE);
    }
  }

  @Test
  public void getBrandWipListTest() {
    Mockito.when(this.pcbFeign.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE)).thenReturn(brandWipResponseGdnRestListResponse);
    GdnRestListResponse<BrandWipResponse>
        response = this.brandWipService.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
    Mockito.verify(this.pcbFeign).getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
    Assertions.assertEquals(DEFAULT_BRAND_NAME, response.getContent().get(0).getBrandName());
    Assertions.assertEquals(DEFAULT_BRAND_DESCRIPTION, response.getContent().get(0).getBrandDescription());
  }

  @Test
  public void getBrandWipHistoryTest() {
    Mockito.when(this.pcbFeign.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE))
        .thenReturn(historyResponse);
    GdnRestListResponse<BrandWipHistoryResponse> response =
        this.brandWipService.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
    Mockito.verify(this.pcbFeign).getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getContent().get(0).getBrandRequestCode());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertEquals(DEFAULT_BRAND_DESCRIPTION, response.getContent().get(0).getDescription());
  }

  @Test
  public void getBrandWipList_expectException() {
    Mockito.when(this.pcbFeign.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE)).thenReturn(null);
    try{
      GdnRestListResponse<BrandWipResponse>
          response = this.brandWipService.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(this.pcbFeign).getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
    }
  }

  @Test
  public void updateTest() throws Exception {
    approveBrandWipWebRequest.setValidBrand(true);
    approveBrandWipWebRequest.setProtectedBrand(true);
    Mockito.doNothing().when(fileStorageService)
        .createBrandLogoFile(brandApproveRequest, multipartFile, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.doNothing().when(fileStorageService)
        .createBrandProfileBannerFile(brandApproveRequest, multipartFile, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.when(this.pcbFeign.update(Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    MultipartFile multipartFile = generateMultipartFile();
    GdnBaseRestResponse response = brandWipService.update(approveBrandWipWebRequest, multipartFile, multipartFile);
    Mockito.verify(this.pcbFeign).update(brandApproveRequestArgumentCaptor.capture());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertNull(response.getErrorMessage());
    Assertions.assertNull(response.getErrorCode());
    Assertions.assertTrue(brandApproveRequestArgumentCaptor.getValue().isValidBrand());
    Assertions.assertTrue(brandApproveRequestArgumentCaptor.getValue().isProtectedBrand());
  }

  @Test
  public void update_expectException() throws Exception {
    Mockito.when(this.pcbFeign.update(Mockito.any())).thenReturn(null);
    MultipartFile multipartFile = generateMultipartFile();
    try {
      brandWipService.update(approveBrandWipWebRequest, multipartFile, multipartFile);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(this.pcbFeign).update(Mockito.any());
    }
  }

  @Test
  public void rejectBrandTest() {
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerSource()).thenReturn(DEFAULT_PROFILE_BANNER_PATH);
    Mockito.when(pcbFeign.rejectBrand(DEFAULT_BRAND_CODE, brandRejectRequest))
        .thenReturn(new GdnRestSingleResponse(brandWipResponse1, DEFAULT_REQUEST_ID));
    String response = brandWipService.rejectBrand(brandRejectRequest);
    Mockito.verify(pcbFeign).rejectBrand(DEFAULT_BRAND_CODE, brandRejectRequest);
    Mockito.verify(systemParameterProperties).getDirectoryBrandLogoSource();
    Mockito.verify(systemParameterProperties).getDirectoryProfileBannerSource();
    Assertions.assertEquals(DEFAULT_BRAND_CODE, response);
  }

  @Test
  public void rejectBrandExceptionTest() {
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerSource()).thenReturn(DEFAULT_PROFILE_BANNER_PATH);
    Mockito.when(pcbFeign.rejectBrand(DEFAULT_BRAND_CODE, brandRejectRequest))
        .thenReturn(new GdnRestSingleResponse(brandWipResponse1, DEFAULT_REQUEST_ID));
    try {
      brandWipService.rejectBrand(brandRejectRequest);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(pcbFeign).rejectBrand(DEFAULT_BRAND_CODE, brandRejectRequest);
      Mockito.verify(systemParameterProperties).getDirectoryBrandLogoSource();
      Mockito.verify(systemParameterProperties).getDirectoryProfileBannerSource();
    }
  }
}