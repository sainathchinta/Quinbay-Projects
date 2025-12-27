package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.properties.QRCodeProperties;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;
import com.gdn.partners.pcu.external.web.model.request.QRDownloadWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QRGenerateRequest;
import com.gdn.partners.pcu.external.web.model.request.QRItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QRProductWebRequest;
import com.google.common.collect.ImmutableMap;
import com.google.zxing.WriterException;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

public class QRServiceImplTest {

  private static final String PRODUCT_SKU_CODE = "Dummy-Product-Code";
  private static final String DOT = ".";
  private static final String PDF = ".pdf";
  private static final String QR_FILE = "qrPin.png";
  private static final String PRODUCT_SKU_NAME = "Dummy Product Name";
  private static final String MERCHANT_CODE = "MTA-Dummy-1200";
  private static final String MERCHANT_NAME = "Dummy Merchant Name";
  private static final String PATH = "./MTA-Dummy-1200";
  private static final String resourcePath = "./src/test/resources/";
  private static final String basePath = "./src/test/resources/testFolders";
  private static final String folder1Path = "./src/test/resources/testFolders/Folder1/";
  private static final String folder2Path = "./src/test/resources/testFolders/Folder2/";
  private static final String fileName1 = "file1.png";
  private static final String fileName2 = "file2.png";
  private static final String DATASTORE = "/data";
  private static final String FILESTORE = "/filestore";
  private static final String TEMPLATE_QR = "template_QR.png";
  private static final String A1_PNG = "A1.png";
  private static final String A5_PNG = "A5.png";
  private static final String A5_PDF = "A5.pdf";
  private static final String BY46_PNG = "BY46.png";
  private static final String BY712_PNG = "BY712.png";
  private static final String PREFIX_URL_PRODUCT =
      "https://www.blibli.com/p/p/productSku?campaign_source=merchant_store_scan";
  private static final String URL_PRODUCT =
      "https://www.blibli.com/p/p/ps--Dummy-Product-Code?campaign_source=merchant_store_scan";
  private static final String URL_MERCHANT =
      "https://www.blibli.com/merchant/MTA-Dummy-1200/dummy-merchant-name?campaign_source=merchant_store_scan";
  private static final String PREFIX_URL_MERCHANT =
      "https://www.blibli.com/merchant/merchantCode/merchantName?campaign_source=merchant_store_scan";
  private static QRProductWebRequest qrProductWebRequest1 = null;
  private static QRProductWebRequest qrProductWebRequest2 = null;
  private static QRItemWebRequest qrItemWebRequest1 = null;
  private static QRItemWebRequest qrItemWebRequest2 = null;
  private static QRItemWebRequest qrItemWebRequest3 = null;
  private static QRItemWebRequest qrItemWebRequest4 = null;
  private static List<QRProductWebRequest> productList = null;
  private static List<QRItemWebRequest> itemList1 = null;
  private static List<QRItemWebRequest> itemList2 = null;
  private static final String PRODUCT_NAME1 = "product-name1";
  private static final String PRODUCT_SKU_CODE1 = "product-skucode1";
  private static final String PRODUCT_NAME2 = "product-name2";
  private static final String PRODUCT_SKU_CODE2 = "product-skucode2";
  private static final String ITEM_NAME1 = "item-name1";
  private static final String ITEM_SKU_CODE1 = "item-skucode1";
  private static final String ITEM_NAME2 = "item-name2";
  private static final String ITEM_SKU_CODE2 = "item-skucode2";
  private static final String ITEM_NAME3 = "item-name3";
  private static final String ITEM_SKU_CODE3 = "item-skucode3";
  private static final String ITEM_NAME4 = "item-name4";
  private static final String ITEM_SKU_CODE4 = "item-skucode4";
  private static final String pdfType = "pdf";
  private static final String pngType = "png";


  @Mock
  private QRCodeProperties qrCodeProperties;

  @InjectMocks
  private QRServiceImpl qrService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private ProductService productService;

  private QRGenerateRequest qrGenerateRequest;

  private QRDownloadWebRequest qrDownloadWebRequest;

  private GdnBaseRestResponse gdnBaseRestResponse;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    qrGenerateRequest = new QRGenerateRequest();
    qrGenerateRequest.setMerchantCode(MERCHANT_CODE);
    qrGenerateRequest.setMerchantName(MERCHANT_NAME);

    qrDownloadWebRequest = new QRDownloadWebRequest();
    qrDownloadWebRequest.setMerchantCode(MERCHANT_CODE);

    qrProductWebRequest1 = new QRProductWebRequest();
    qrProductWebRequest1.setProductName(PRODUCT_NAME1);
    qrProductWebRequest1.setProductSku(PRODUCT_SKU_CODE1);

    qrItemWebRequest1 = new QRItemWebRequest();
    qrItemWebRequest1.setItemName(ITEM_NAME1);
    qrItemWebRequest1.setItemSku(ITEM_SKU_CODE1);

    qrItemWebRequest2 = new QRItemWebRequest();
    qrItemWebRequest2.setItemName(ITEM_NAME2);
    qrItemWebRequest2.setItemSku(ITEM_SKU_CODE2);

    itemList1 = new ArrayList<>();
    itemList1.add(qrItemWebRequest1);
    itemList1.add(qrItemWebRequest2);

    qrProductWebRequest2 = new QRProductWebRequest();
    qrProductWebRequest2.setProductName(PRODUCT_NAME2);
    qrProductWebRequest2.setProductSku(PRODUCT_SKU_CODE2);

    qrItemWebRequest3 = new QRItemWebRequest();
    qrItemWebRequest3.setItemName(ITEM_NAME3);
    qrItemWebRequest3.setItemSku(ITEM_SKU_CODE3);

    qrItemWebRequest4 = new QRItemWebRequest();
    qrItemWebRequest4.setItemName(ITEM_NAME4);
    qrItemWebRequest4.setItemSku(ITEM_SKU_CODE4);

    itemList2 = new ArrayList<>();
    itemList2.add(qrItemWebRequest3);
    itemList2.add(qrItemWebRequest4);

    qrProductWebRequest1.setItems(itemList1);
    qrProductWebRequest2.setItems(itemList2);

    productList =new ArrayList<>();
    productList.add(qrProductWebRequest1);
    productList.add(qrProductWebRequest2);

    qrDownloadWebRequest.setProducts(productList);

    gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(Boolean.TRUE);

    Map<String, String> itemNameMap =
        ImmutableMap.of(ITEM_SKU_CODE1, ITEM_NAME1, ITEM_SKU_CODE2, ITEM_NAME2, ITEM_SKU_CODE3, ITEM_NAME3,
            ITEM_SKU_CODE4, ITEM_NAME4);
    when(productService.fetchItemNamesByItemSku(Mockito.anyList())).thenReturn(itemNameMap);
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  public void merchantUrlTest() {
    Mockito.when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(PREFIX_URL_MERCHANT);
    String url = qrService.generateURL(qrGenerateRequest, true);
    Assertions.assertEquals(URL_MERCHANT, url);
    Mockito.verify(qrCodeProperties).getPrefixUrlMerchant();
  }

  @Test
  public void productUrlTest() {
    qrGenerateRequest.setProductSkuCode(PRODUCT_SKU_CODE);
    qrGenerateRequest.setProductSkuName(PRODUCT_SKU_NAME);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    String url = qrService.generateURL(qrGenerateRequest, true);
    Assertions.assertEquals(URL_PRODUCT, url);
    Mockito.verify(qrCodeProperties).getPrefixUrlProduct();
  }

  @Test
  public void merchantQRHandlerTest() throws IOException, WriterException {
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(PREFIX_URL_MERCHANT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    String result = qrService.getPathOfImage(qrGenerateRequest);
    Mockito.verify(qrCodeProperties).getBasePath();
    Mockito.verify(qrCodeProperties).getTemplatePath();
    Mockito.verify(qrCodeProperties).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties).getPrefixUrlMerchant();
    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.startsWith(DATASTORE));
    Assertions.assertFalse(result.startsWith(FILESTORE));
    Assertions.assertTrue(result.contains(TEMPLATE_QR));
  }

  @Test
  public void productQRHandlerTest() throws IOException, WriterException {
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    qrGenerateRequest.setProductSkuCode(PRODUCT_SKU_CODE);
    qrGenerateRequest.setProductSkuName(PRODUCT_SKU_NAME);
    String result = qrService.getPathOfImage(qrGenerateRequest);
    Mockito.verify(qrCodeProperties).getBasePath();
    Mockito.verify(qrCodeProperties).getTemplatePath();
    Mockito.verify(qrCodeProperties).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties).getBlibliPinSmall();
    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.startsWith(DATASTORE));
    Assertions.assertFalse(result.startsWith(FILESTORE));
    Assertions.assertTrue(result.contains(TEMPLATE_QR));
  }

  @Test
  public void merchantTemplateHandlerA5Test() throws IOException, WriterException {
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(PREFIX_URL_MERCHANT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    qrGenerateRequest.setTemplateSize(TemplateSize.A5);
    qrGenerateRequest.setIsDarkTheme(true);
    qrGenerateRequest.setIsCnC(false);
    String result = qrService.getPathOfImage(qrGenerateRequest);
    Mockito.verify(qrCodeProperties, times(2)).getBasePath();
    Mockito.verify(qrCodeProperties, times(2)).getTemplatePath();
    Mockito.verify(qrCodeProperties).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties).getPrefixUrlMerchant();
    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.startsWith(DATASTORE));
    Assertions.assertFalse(result.startsWith(FILESTORE));
    Assertions.assertTrue(result.contains(A5_PNG));
  }

  @Test
  public void merchantTemplateHandlerA1Test() throws IOException, WriterException {
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(PREFIX_URL_MERCHANT);
    Mockito.when(qrCodeProperties.getBlibliPinBig()).thenReturn(QR_FILE);
    qrGenerateRequest.setTemplateSize(TemplateSize.A1);
    qrGenerateRequest.setIsDarkTheme(true);
    qrGenerateRequest.setIsCnC(false);
    String result = qrService.getPathOfImage(qrGenerateRequest);
    Mockito.verify(qrCodeProperties, times(2)).getBasePath();
    Mockito.verify(qrCodeProperties, times(2)).getTemplatePath();
    Mockito.verify(qrCodeProperties).getBlibliPinBig();
    Mockito.verify(qrCodeProperties).getPrefixUrlMerchant();
    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.startsWith(DATASTORE));
    Assertions.assertFalse(result.startsWith(FILESTORE));
    Assertions.assertTrue(result.contains(A1_PNG));
  }

  @Test
  public void productTemplateHandlerBY46Test() throws IOException, WriterException {
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    qrGenerateRequest.setTemplateSize(TemplateSize.BY46);
    qrGenerateRequest.setIsDarkTheme(true);
    qrGenerateRequest.setProductSkuName(PRODUCT_SKU_NAME);
    qrGenerateRequest.setProductSkuCode(PRODUCT_SKU_CODE);
    String result = qrService.getPathOfImage(qrGenerateRequest);
    Mockito.verify(qrCodeProperties, times(2)).getBasePath();
    Mockito.verify(qrCodeProperties, times(2)).getTemplatePath();
    Mockito.verify(qrCodeProperties).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties).getBlibliPinSmall();
    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.startsWith(DATASTORE));
    Assertions.assertFalse(result.startsWith(FILESTORE));
    Assertions.assertTrue(result.contains(BY46_PNG));
  }

  @Test
  public void productTemplateHandlerBY712Test() throws IOException, WriterException {
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    qrGenerateRequest.setTemplateSize(TemplateSize.BY712);
    qrGenerateRequest.setIsDarkTheme(true);
    qrGenerateRequest.setProductSkuName(PRODUCT_SKU_NAME);
    qrGenerateRequest.setProductSkuCode(PRODUCT_SKU_CODE);
    String result = qrService.getPathOfImage(qrGenerateRequest);
    Mockito.verify(qrCodeProperties, times(2)).getBasePath();
    Mockito.verify(qrCodeProperties).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties, times(2)).getTemplatePath();
    Mockito.verify(qrCodeProperties).getPrefixUrlProduct();
    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.startsWith(DATASTORE));
    Assertions.assertFalse(result.startsWith(FILESTORE));
    Assertions.assertTrue(result.contains(BY712_PNG));
  }

  @Test
  public void merchantTemplateDownloadTest() throws IOException, WriterException {
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(PREFIX_URL_MERCHANT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    qrGenerateRequest.setTemplateSize(TemplateSize.A5);
    qrGenerateRequest.setIsDarkTheme(true);
    qrGenerateRequest.setIsCnC(false);
    String result = qrService.merchantTemplateDownload(qrGenerateRequest);
    Mockito.verify(qrCodeProperties, times(2)).getBasePath();
    Mockito.verify(qrCodeProperties,times(2)).getTemplatePath();
    Mockito.verify(qrCodeProperties).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties).getPrefixUrlMerchant();
    Assertions.assertTrue(result.endsWith(PDF));
    Assertions.assertFalse(result.startsWith(DATASTORE));
    Assertions.assertFalse(result.startsWith(FILESTORE));
    Assertions.assertTrue(result.contains(A5_PDF));
  }

  @Test
  public void deleteQRCodesTest() throws Exception{
    createDirectory();
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(basePath );
    qrService.deleteQRCodes(0);
    Mockito.verify(qrCodeProperties, times(1)).getBasePath();
  }

  @Test
  public void downloadQRCodesForProducts_BY46_darkTheme_productLevelPdfTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY46);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(true);
    qrDownloadWebRequest.setIsDarkTheme(true);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(6)).getBasePath();
    Mockito.verify(qrCodeProperties, times(4)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(2)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(2)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY46_lightTheme_productLevelPdfTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY46);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(true);
    qrDownloadWebRequest.setIsDarkTheme(false);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(6)).getBasePath();
    Mockito.verify(qrCodeProperties, times(4)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(2)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(2)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY46_productLevelPdf_productNameNullTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY46);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(true);
    qrDownloadWebRequest.setIsDarkTheme(true);
    qrDownloadWebRequest.getProducts().get(0).setProductName(null);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(4)).getBasePath();
    Mockito.verify(qrCodeProperties, times(2)).getTemplatePath();
    Mockito.verify(qrCodeProperties).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY712_darkTheme_productLevelPdfTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY712);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(true);
    qrDownloadWebRequest.setIsDarkTheme(true);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(6)).getBasePath();
    Mockito.verify(qrCodeProperties, times(4)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(2)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(2)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY712_lightTheme_productLevelPdfTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY712);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(true);
    qrDownloadWebRequest.setIsDarkTheme(false);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(6)).getBasePath();
    Mockito.verify(qrCodeProperties, times(4)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(2)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(2)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_pngTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(null);
    qrDownloadWebRequest.setType(pngType);
    qrDownloadWebRequest.setProductLevel(true);
    qrDownloadWebRequest.setIsDarkTheme(null);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(4)).getBasePath();
    Mockito.verify(qrCodeProperties, times(2)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(2)).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties, times(2)).getTemplatePath();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_itemLevel_pngTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(null);
    qrDownloadWebRequest.setType(pngType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(null);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(6)).getBasePath();
    Mockito.verify(qrCodeProperties, times(4)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(4)).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties, times(4)).getTemplatePath();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProductsImageItemNameEmptyTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    Mockito.when(productService.fetchItemNamesByItemSku(Mockito.anyList())).thenReturn(ImmutableMap.of(ITEM_SKU_CODE1, ITEM_NAME1));
    qrDownloadWebRequest.setTemplateSize(null);
    qrDownloadWebRequest.setType(pngType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(false);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(3)).getBasePath();
    Mockito.verify(qrCodeProperties, times(1)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(1)).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties, times(1)).getTemplatePath();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_png_withNullProductName() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(null);
    qrDownloadWebRequest.setType(pngType);
    qrDownloadWebRequest.setProductLevel(true);
    qrDownloadWebRequest.setIsDarkTheme(null);
    qrDownloadWebRequest.getProducts().get(0).setProductName(null);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(3)).getBasePath();
    Mockito.verify(qrCodeProperties, times(1)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(1)).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties, times(1)).getTemplatePath();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_png_withNullItemName() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(null);
    qrDownloadWebRequest.setType(pngType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(null);
    qrDownloadWebRequest.getProducts().get(0).getItems().get(0).setItemName(null);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(6)).getBasePath();
    Mockito.verify(qrCodeProperties, times(4)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(4)).getBlibliPinSmall();
    Mockito.verify(qrCodeProperties, times(4)).getTemplatePath();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY46_darkTheme_itemLevelPdfTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY46);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(true);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(10)).getBasePath();
    Mockito.verify(qrCodeProperties, times(8)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(4)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(4)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY46_lightTheme_itemLevelPdfTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY46);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(false);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(10)).getBasePath();
    Mockito.verify(qrCodeProperties, times(8)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(4)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(4)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY46_itemLevelPdf_forItemNameNullTest() throws Exception{
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY46);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(true);
    qrDownloadWebRequest.getProducts().get(0).getItems().get(0).setItemName(null);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(10)).getBasePath();
    Mockito.verify(qrCodeProperties, times(8)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(4)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(4)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY712_darkTheme_itemLevelPdfTest() throws Exception{
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY712);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(true);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(10)).getBasePath();
    Mockito.verify(qrCodeProperties, times(8)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(4)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(4)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProducts_BY712_lightTheme_itemLevelPdfTest() throws Exception{
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY712);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(false);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(10)).getBasePath();
    Mockito.verify(qrCodeProperties, times(8)).getTemplatePath();
    Mockito.verify(qrCodeProperties, times(4)).getPrefixUrlProduct();
    Mockito.verify(qrCodeProperties, times(4)).getBlibliPinSmall();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  @Test
  public void downloadQRCodesForProductsPdfItemNameEmptyTest() throws Exception{
    Mockito.when(pbpFeign.generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString())).thenReturn(gdnBaseRestResponse);
    Mockito.when(qrCodeProperties.getBasePath()).thenReturn(DOT);
    Mockito.when(qrCodeProperties.getBlibliPinSmall()).thenReturn(QR_FILE);
    Mockito.when(qrCodeProperties.getTemplatePath()).thenReturn(resourcePath);
    Mockito.when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PREFIX_URL_PRODUCT);
    when(productService.fetchItemNamesByItemSku(Mockito.anyList())).thenReturn(new HashMap());
    qrDownloadWebRequest.setTemplateSize(TemplateSize.BY712);
    qrDownloadWebRequest.setType(pdfType);
    qrDownloadWebRequest.setProductLevel(false);
    qrDownloadWebRequest.setIsDarkTheme(false);
    qrService.downloadQRCodesForProducts(qrDownloadWebRequest);
    Mockito.verify(qrCodeProperties, times(2)).getBasePath();
    Mockito.verify(pbpFeign).generateQRCodeNotification(Mockito.eq(MERCHANT_CODE), Mockito.anyString());
  }

  private void createDirectory() throws IOException {
    File mainDirectory = new File(basePath);
    mainDirectory.mkdir();
    File directory1 = new File(folder1Path);
    directory1.mkdir();
    File file1 = new File(folder1Path + fileName1);
    File file2 = new File(folder1Path + fileName2);
    file1.createNewFile();
    file2.createNewFile();

    File directory2 = new File(folder2Path);
    directory2.mkdir();
    File file3 = new File(folder2Path + fileName1);
    File file4 = new File(folder2Path + fileName2);
    file3.createNewFile();
    file4.createNewFile();
  }

  @AfterEach
  public void tearDown() throws Exception {
    deleteFolder(PATH);
    deleteFolder(basePath);
    Mockito.verifyNoMoreInteractions(qrCodeProperties);
  }
}
