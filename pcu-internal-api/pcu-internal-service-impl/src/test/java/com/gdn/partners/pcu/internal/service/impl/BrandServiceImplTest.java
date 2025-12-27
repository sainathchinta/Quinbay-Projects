package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.util.ResourceUtils;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.exception.ApplicationException;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.web.model.request.BrandDeleteWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandlogoPath;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BrandServiceImplTest {

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private FileStorageService fileStorageService;

  @InjectMocks
  private BrandServiceImpl brandService;

  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_DESCRIPTION = "description";
  private static final String BRAND_DELETE_REASON = "brandDeleteReason";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.jpg";
  private static final String DEFAULT_PROFILE_BANNER_PATH = "blibli-com-logo.jpg";
  private static final String REQUEST_ID = "requestId";
  private static final String BRAND_DESCRIPTION1 = "brandDescription";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String GENERATED_BRAND_LOGO_PATH = "brandname-logo.jpg";
  private static final String BRAND_LOGO_PATH1 = "/" + BRAND_REQUEST_CODE + "/" + GENERATED_BRAND_LOGO_PATH;
  private static final String BRAND_DIRECTORY_LOGO_FINAL = "brandDirectoryLogoFinal";
  private static final String PROFILE_DIRECTORY_BANNER_FINAL = "profileDirectoryBannerFinal";
  private int height;
  private int width;
  private BrandDeleteWebRequest brandDeleteWebRequest;
  private GdnRestSingleResponse<BrandResponse> brandResponseGdn;
  private GdnRestSingleResponse<BrandWipResponse> brandWipResponseGdn;
  private GdnRestSingleResponse<UpdateBrandlogoPath> updateBrandlogoPath;
  private BrandResponse brandResponse = new BrandResponse();
  private BrandWipResponse brandWipResponse = new BrandWipResponse();
  private static MockMultipartFile multipartFile;
  private static String filePath2;
  private static File file;
  private static BufferedImage img;
  private UpdateBrandRequest updateBrandRequest;
  private GdnRestSingleResponse<BrandWipResponse> gdnRestSingleResponse;
  private BrandWipResponse brandWipResponse1;
  private GdnBaseRestResponse gdnBaseRestResponse;

  @BeforeEach
  public void setUp() throws Exception {
    brandDeleteWebRequest = BrandDeleteWebRequest.builder().brandName(BRAND_NAME)
        .brandDeletedReason(BRAND_DELETE_REASON).build();
    brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setBrandDescription(BRAND_NAME);
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setBrandDescription(BRAND_DESCRIPTION);
    generateMultipartFile();
    String filePath = ResourceUtils.getFile("classpath:Brand/" + DEFAULT_BRAND_LOGO_PATH).toString();
    generateMultipartFileProfileBanner();
    filePath2 = ResourceUtils.getFile("classpath:Brand/" + DEFAULT_PROFILE_BANNER_PATH).toString();
    brandResponse.setBrandLogoPath(filePath);
    brandResponse.setProfileBannerPath(filePath2);
    brandResponseGdn = new GdnRestSingleResponse<>(brandResponse, REQUEST_ID);
    brandWipResponse.setBrandName(BRAND_NAME);
    brandWipResponse.setBrandCode(BRAND_CODE);
    brandWipResponse.setBrandDescription(BRAND_DESCRIPTION);
    brandWipResponse.setBrandLogoPath(filePath);
    brandWipResponseGdn = new GdnRestSingleResponse<>(brandWipResponse, REQUEST_ID);

    updateBrandRequest = new UpdateBrandRequest();
    updateBrandRequest.setBrandCode(BRAND_CODE);
    updateBrandRequest.setBrandName(BRAND_NAME);
    updateBrandRequest.setBrandDescription(BRAND_DESCRIPTION1);
    updateBrandRequest.setBrandLogoPath(BrandServiceImplTest.BRAND_LOGO_PATH1);
    updateBrandRequest.setProfileBannerPath(BrandServiceImplTest.BRAND_LOGO_PATH1);

    updateBrandlogoPath = new GdnRestSingleResponse<>();
    updateBrandlogoPath.setSuccess(true);
    UpdateBrandlogoPath updateBrandlogoPath1 = new UpdateBrandlogoPath();
    updateBrandlogoPath1.setBrandLogoPath(BrandServiceImplTest.BRAND_LOGO_PATH1);
    updateBrandlogoPath.setValue(updateBrandlogoPath1);

    gdnRestSingleResponse = new GdnRestSingleResponse<>();
    gdnRestSingleResponse.setSuccess(true);
    brandWipResponse1 = new BrandWipResponse();
    brandWipResponse1.setBrandCode(BRAND_CODE);
    brandWipResponse1.setBrandRequestCode(BRAND_REQUEST_CODE);
    gdnRestSingleResponse.setValue(brandWipResponse1);

    gdnBaseRestResponse = new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, REQUEST_ID);
  }

  @Test
  public void deleteBrandTest() {
    when(pbpFeign.getProductsCountByBrandName(BRAND_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, 0L));
    when(pcbFeign.deleteBrand(BRAND_CODE, brandDeleteWebRequest.getBrandDeletedReason()))
        .thenReturn(gdnBaseRestResponse);
    brandService.deleteBrand(BRAND_CODE, brandDeleteWebRequest);
    verify(pbpFeign).getProductsCountByBrandName(BRAND_NAME);
    verify(pcbFeign).deleteBrand(BRAND_CODE, brandDeleteWebRequest.getBrandDeletedReason());
  }

  @Test
  public void deleteBrandWithProductsMappedTest() {
    try {
      when(pbpFeign.getProductsCountByBrandName(BRAND_NAME))
          .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, 10L));
      brandService.deleteBrand(BRAND_CODE, brandDeleteWebRequest);
    }catch (InvalidStateException e) {

    } finally {
      verify(pbpFeign).getProductsCountByBrandName(BRAND_NAME);
    }
  }

  @Test
  public void findBrandLogoByBrandCodeTest() throws Exception {
    Mockito.when(this.pcbFeign.filterByBrandCode(BRAND_CODE)).thenReturn(brandResponseGdn);
    this.brandService.filterBrandLogoByBrandCode(BRAND_CODE, true);
    Mockito.verify(this.fileStorageService).getBrandImage(brandWipResponse.getBrandCode(),brandWipResponse.getBrandLogoPath(), true, true);
    Mockito.verify(this.pcbFeign).filterByBrandCode(BRAND_CODE);
  }

  @Test
  public void findBrandLogoByBrandCodeTest_expectApplicationException() throws Exception {
    brandResponseGdn.setValue(null);
    Mockito.when(this.pcbFeign.filterByBrandCode(BRAND_CODE)).thenReturn(brandResponseGdn);
    try {
      this.brandService.filterBrandLogoByBrandCode(BRAND_CODE, true);
    } catch (ApplicationException e) {
    }
    finally {
      Mockito.verify(this.pcbFeign).filterByBrandCode(BRAND_CODE);
    }
  }

  @Test
  public void findBrandLogoByBrandRequestCodeTest() throws Exception{
    brandWipResponseGdn.getValue().setBrandRequestCode(StringUtils.EMPTY);
    Mockito.when(this.pcbFeign.filterByBrandRequestCode(BRAND_CODE)).thenReturn(brandWipResponseGdn);
    this.brandService.filterBrandLogoByBrandRequestCode(BRAND_CODE, true);
    Mockito.verify(this.fileStorageService).getBrandImage(brandWipResponseGdn.getValue().getBrandRequestCode(),brandWipResponse.getBrandLogoPath(), false, true);
    Mockito.verify(this.pcbFeign).filterByBrandRequestCode(BRAND_CODE);
  }

  @Test
  public void findBrandLogoByBrandRequestCode_approvedBrandWipTest() throws Exception{
    brandWipResponseGdn.getValue().setBrandCode(StringUtils.EMPTY);
    brandWipResponseGdn.getValue().setState(BrandWipState.APPROVED.getDescription());
    Mockito.when(this.pcbFeign.filterByBrandRequestCode(BRAND_CODE)).thenReturn(brandWipResponseGdn);
    this.brandService.filterBrandLogoByBrandRequestCode(BRAND_CODE, true);
    Mockito.verify(this.fileStorageService).getBrandImage(brandWipResponse.getBrandCode(),brandWipResponse.getBrandLogoPath(), true, true);
    Mockito.verify(this.pcbFeign).filterByBrandRequestCode(BRAND_CODE);
  }

  @Test
  public void findBrandLogoByBrandRequestCode_expectException() throws Exception {
    brandWipResponseGdn.setValue(null);
    Mockito.when(this.pcbFeign.filterByBrandRequestCode(BRAND_CODE)).thenReturn(brandWipResponseGdn);
    try {
      this.brandService.filterBrandLogoByBrandRequestCode(BRAND_CODE, true);
    } catch (ApplicationException e){
    } finally {
      Mockito.verify(this.pcbFeign).filterByBrandRequestCode(BRAND_CODE);
    }
  }

  @Test
  public void findProfileBannerByBrandCodeTest() throws Exception {
    Mockito.when(this.pcbFeign.filterByBrandCode(BRAND_CODE)).thenReturn(brandResponseGdn);
    this.brandService.filterBrandLogoByBrandCode(BRAND_CODE, false);
    Mockito.verify(this.fileStorageService).getBrandImage(brandWipResponse.getBrandCode(),brandWipResponse.getBrandLogoPath(), true, false);
    Mockito.verify(this.pcbFeign).filterByBrandCode(BRAND_CODE);
  }

  @Test
  public void findProfileBannerByBrandCodeTest_expectApplicationException() throws Exception {
    brandResponseGdn.setValue(null);
    Mockito.when(this.pcbFeign.filterByBrandCode(BRAND_CODE)).thenReturn(brandResponseGdn);
    try {
      this.brandService.filterBrandLogoByBrandCode(BRAND_CODE, false);
    } catch (ApplicationException e) {
    }
    finally {
      Mockito.verify(this.pcbFeign).filterByBrandCode(BRAND_CODE);
    }
  }

  @Test
  public void findProfileBannerByBrandRequestCodeTest() throws Exception{
    brandWipResponseGdn.getValue().setBrandRequestCode(StringUtils.EMPTY);
    brandWipResponseGdn.getValue().setProfileBannerPath(filePath2);
    brandWipResponseGdn.getValue().setBrandRequestCode(StringUtils.EMPTY);
    Mockito.when(this.pcbFeign.filterByBrandRequestCode(BRAND_CODE)).thenReturn(brandWipResponseGdn);
    this.brandService.filterBrandLogoByBrandRequestCode(BRAND_CODE, false);
    Mockito.verify(this.pcbFeign).filterByBrandRequestCode(BRAND_CODE);
  }

  @Test
  public void findProfileBannerByBrandRequestCode_approvedBrandWipTest() throws Exception{
    brandWipResponseGdn.getValue().setBrandCode(StringUtils.EMPTY);
    brandWipResponseGdn.getValue().setState(BrandWipState.APPROVED.getDescription());
    brandWipResponseGdn.getValue().setProfileBannerPath(filePath2);
    Mockito.when(this.pcbFeign.filterByBrandRequestCode(BRAND_CODE)).thenReturn(brandWipResponseGdn);
    this.brandService.filterBrandLogoByBrandRequestCode(BRAND_CODE, false);
    Mockito.verify(this.fileStorageService).getBrandImage(brandWipResponse.getBrandCode(),brandWipResponse.getBrandLogoPath(), true, false);
    Mockito.verify(this.pcbFeign).filterByBrandRequestCode(BRAND_CODE);
  }

  @Test
  public void findProfileBannerByBrandRequestCode_expectException() throws Exception {
    brandWipResponseGdn.setValue(null);
    Mockito.when(this.pcbFeign.filterByBrandRequestCode(BRAND_CODE)).thenReturn(brandWipResponseGdn);
    try {
      this.brandService.filterBrandLogoByBrandRequestCode(BRAND_CODE, false);
    } catch (ApplicationException e){
    } finally {
      Mockito.verify(this.pcbFeign).filterByBrandRequestCode(BRAND_CODE);
    }
  }


  @Test
  public void getBrandDetailTest() {
    brandResponse.setValidBrand(true);
    when(pcbFeign.getBrandDetail(BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(brandResponse, REQUEST_ID));
    BrandWebResponse response = brandService.getBrandDetail(BRAND_CODE);
    verify(pcbFeign).getBrandDetail(BRAND_CODE);
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BRAND_NAME, response.getBrandName());
    Assertions.assertEquals(BRAND_DESCRIPTION, response.getBrandDescription());
    Assertions.assertTrue(response.isValidBrand());
  }

  @Test
  public void updateBrandTest() throws Exception {
    Mockito.when(pcbFeign.getBrandWipByBrandCode(BRAND_CODE)).thenReturn(gdnRestSingleResponse);
    Mockito.when(pcbFeign.updateBrand(Mockito.any(UpdateBrandRequest.class))).thenReturn(updateBrandlogoPath);
    MultipartFile multipartFile = generateMultipartFile();
    Mockito.doNothing().when(fileStorageService)
        .updateBrandFiles(updateBrandRequest, multipartFile, multipartFile, BRAND_REQUEST_CODE);
    Mockito.doNothing().when(fileStorageService)
        .deleteUpdatedBrandLogo(PROFILE_DIRECTORY_BANNER_FINAL, BRAND_REQUEST_CODE);
    brandService.updateBrand(updateBrandRequest, multipartFile, multipartFile);
    Mockito.verify(pcbFeign).getBrandWipByBrandCode(BRAND_CODE);
    Mockito.verify(pcbFeign).updateBrand(Mockito.any(UpdateBrandRequest.class));
    deleteFile("target/test-classes/Brand");
    deleteFile(BRAND_DIRECTORY_LOGO_FINAL);
    deleteFile(PROFILE_DIRECTORY_BANNER_FINAL);
  }


  @Test
  void updateBrandDeletionNoBrandImageUpdateTest() throws Exception {
    Mockito.when(pcbFeign.getBrandWipByBrandCode(BRAND_CODE)).thenReturn(gdnRestSingleResponse);
    Mockito.when(pcbFeign.updateBrand(Mockito.any(UpdateBrandRequest.class))).thenReturn(updateBrandlogoPath);
    MultipartFile multipartFile = generateMultipartFile();
    Mockito.doNothing().when(fileStorageService)
      .updateBrandFiles(updateBrandRequest, multipartFile, multipartFile, BRAND_REQUEST_CODE);
    Mockito.doNothing().when(fileStorageService)
      .deleteUpdatedBrandLogo(PROFILE_DIRECTORY_BANNER_FINAL, BRAND_REQUEST_CODE);
    brandService.updateBrand(updateBrandRequest, null, multipartFile);
    Mockito.verify(pcbFeign).getBrandWipByBrandCode(BRAND_CODE);
    Mockito.verify(pcbFeign).updateBrand(Mockito.any(UpdateBrandRequest.class));
    deleteFile("target/test-classes/Brand");
    deleteFile(BRAND_DIRECTORY_LOGO_FINAL);
    deleteFile(PROFILE_DIRECTORY_BANNER_FINAL);
  }

  @Test
  public void updateBrandTest_expectException() throws Exception {
    gdnRestSingleResponse.setSuccess(false);
    gdnRestSingleResponse.setValue(null);
    Mockito.when(pcbFeign.getBrandWipByBrandCode(BRAND_CODE)).thenReturn(gdnRestSingleResponse);
    try {
      brandService.updateBrand(updateBrandRequest, generateMultipartFile(), null);
    } catch (Exception e) {
    } finally {
      Mockito.verify(pcbFeign).getBrandWipByBrandCode(BRAND_CODE);
      deleteFile("target/test-classes/Brand");
    }
  }

  private void deleteFile(String destination) throws IOException {
    FileUtils.deleteDirectory(new File(destination));
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pcbFeign);
    verifyNoMoreInteractions(pbpFeign);
  }

  private MultipartFile generateMultipartFile() throws Exception {
    mockFile("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = ResourceUtils.getFile("classpath:Brand/" + DEFAULT_BRAND_LOGO_PATH);
    multipartFile = new MockMultipartFile(DEFAULT_BRAND_LOGO_PATH, DEFAULT_BRAND_LOGO_PATH, "image/jpg",
        IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }

  private MultipartFile generateMultipartFileProfileBanner() throws Exception {
    mockFile("target/test-classes/Brand/" + DEFAULT_PROFILE_BANNER_PATH);
    file = ResourceUtils.getFile("classpath:Brand/" + DEFAULT_PROFILE_BANNER_PATH);
    multipartFile = new MockMultipartFile(DEFAULT_PROFILE_BANNER_PATH, DEFAULT_PROFILE_BANNER_PATH, "image/jpg",
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

}