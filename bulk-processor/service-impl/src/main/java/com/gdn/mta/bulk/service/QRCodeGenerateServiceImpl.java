package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.mta.bulk.config.QRCodeProperties;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.QrCodeRowItemInfo;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.google.cloud.Tuple;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.MultiFormatWriter;
import com.google.zxing.client.j2se.MatrixToImageConfig;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;
import com.openhtmltopdf.pdfboxout.PdfRendererBuilder;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.helper.W3CDom;
import org.jsoup.nodes.Document;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Slf4j
public class QRCodeGenerateServiceImpl implements QRCodeGenerateService {

  @Autowired
  private BulkProcessDataService bulkProcessDataService;
  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private HtmlTemplateParserService htmlTemplateParserService;
  @Autowired
  private FileStorageService fileStorageService;
  @Autowired
  private XProductOutboundService xProductOutboundService;
  @Autowired
  private CMSOutboundService cmsOutboundService;
  @Autowired
  private QRCodeProperties qrCodeProperties;
  @Value("${qrcode.color.rgb}")
  private String qrcodeColorRgb;

  @Value("${final.short.url.prefix}")
  private String finalShortUrlPrefix;

  @Value("#{${url.shortener.switch.map}}")
  private Map<String, Boolean> urlShortenerSwitchMap = new HashMap<>();

  private static final String PNG = "png";
  public static final String DEFAULT_FOLDER = "qr_pdfs";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PS = "ps--";
  private static final String IS = "is--";
  private static final String PICKUP_POINT_QUERY = "&pickupPointCode=%s";
  private static final String CNC_QUERY = "&cnc=true";
  private static final String BASE_64_PNG = "data:image/png;base64,";
  private static final String ITEM_SKU_QUERY = "&id=%s";

  @Override
  public void generateQRCode(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
            bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
            bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      String fileName = String.format("%08d", bulkProcessData.getRowNumber())
          + RandomStringUtils.randomAlphanumeric(8).toUpperCase() + ProcessorUtils.FILETYPE_PDF;

      File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
          File.separator + DEFAULT_FOLDER);
      if (!directory.exists()) {
        directory.mkdir();
      }
      File file = new File(directory.getAbsolutePath() + File.separator + fileName);
      try {
        processPageQR(bulkProcessData, bulkUpdateEventModel, fileName, file);
        bulkProcessData.setEndDate(new Date());
        bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
      } catch (Exception exception) {
        log.error("#QRCodeGenerateService generateQRCode error while processing {} ",
            bulkProcessData, exception);
        setError(bulkProcessData);
      } finally {
        file.delete();
        bulkProcessDataService.saveOperation(bulkProcessData);
      }
    }
  }

  private void setError(BulkProcessData bulkProcessData) {
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData.setInputErrorCount(null);
    bulkProcessData.setSystemErrorCount(1);
    bulkProcessData.setEndDate(new Date());
    bulkProcessData.setErrorMessage(Constant.SYSTEM_ERROR);
  }

  private void processPageQR(BulkProcessData bulkProcessData,
      BulkUpdateEventModel bulkUpdateEventModel, String fileName, File file) throws Exception {
    QrCodeRowInfo qrCodeRowInfo =
        objectMapper.readValue(bulkProcessData.getBulkRequestData(), QrCodeRowInfo.class);
    Map<String, PriceRangeResponse> priceRangeResponses = null;
    if ((qrCodeRowInfo.isBulkExcelUpload() || qrCodeRowInfo.isPrintPrice()) && (
        AllowedQRGenerationType.PRODUCT.getValue().equals(qrCodeRowInfo.getQrGenerationType())
            || AllowedQRGenerationType.ITEM.getValue()
            .equals(qrCodeRowInfo.getQrGenerationType()))) {
      Set<String> skus = qrCodeRowInfo.getRowItems()
          .stream()
          .filter(qrCodeRowItemInfo -> StringUtils.isEmpty(qrCodeRowItemInfo.getProductPrice()))
          .map(qrCodeRowItemInfo -> AllowedQRGenerationType.PRODUCT.getValue()
              .equals(qrCodeRowInfo.getQrGenerationType()) ?
              qrCodeRowItemInfo.getProductSku() :
              qrCodeRowItemInfo.getItemSku())
          .collect(Collectors.toSet());
      if (!CollectionUtils.isEmpty(skus)) {
        List<PriceRangeResponse> responses = xProductOutboundService.fetchPriceRange(
            bulkUpdateEventModel.getBusinessPartnerCode(), skus);
        priceRangeResponses = responses.stream()
            .collect(Collectors.toMap(PriceRangeResponse::getWebSku,
                priceRangeResponse -> priceRangeResponse, (o, o2) -> o));
      }
    }
    for (QrCodeRowItemInfo qrCodeRowItemInfo : qrCodeRowInfo.getRowItems()) {
      processRow(qrCodeRowItemInfo, qrCodeRowInfo, priceRangeResponses);
    }
    String vtl = fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo);
    String html = htmlTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), vtl,
        objectMapper.convertValue(qrCodeRowInfo, Map.class));
    generatePdfFromHtml(html, file);
    String filePath = fileStorageService.getBasePath(BulkProcessType.QR_GENERATION.getValue())
        + bulkUpdateEventModel.getBulkProcessCode() + File.separator + fileName;
    fileStorageService.uploadFileToGcs(filePath, MediaType.APPLICATION_PDF_VALUE,
        new FileInputStream(file));
  }

  private void processRow(QrCodeRowItemInfo qrCodeRowItemInfo, QrCodeRowInfo qrCodeRowInfo,
      Map<String, PriceRangeResponse> priceRangeResponses) throws Exception {
    Tuple<Integer, Integer> measurements = getQrSize(qrCodeRowInfo.getQrPerPage(),qrCodeRowInfo.getTemplateSize());
    qrCodeRowItemInfo.setQrCodeBase64(BASE_64_PNG + qrGenerator(measurements.x(), measurements.y(),
        generateURL(qrCodeRowItemInfo, qrCodeRowInfo), qrCodeRowInfo.getQrPerPage()));
    if ((qrCodeRowInfo.isBulkExcelUpload() || qrCodeRowInfo.isPrintPrice()) && !CollectionUtils.isEmpty(priceRangeResponses)) {
      boolean isProduct =
          AllowedQRGenerationType.PRODUCT.getValue().equals(qrCodeRowInfo.getQrGenerationType());
      for (QrCodeRowItemInfo rowItem : qrCodeRowInfo.getRowItems()) {
        PriceRangeResponse priceRangeResponse =
            priceRangeResponses.get(isProduct ? rowItem.getProductSku() : rowItem.getItemSku());
        if (Objects.nonNull(priceRangeResponse)) {
          if (qrCodeRowInfo.isPrintPrice() && StringUtils.isNotEmpty(
              priceRangeResponse.getPriceRange())) {
            rowItem.setProductPrice(priceRangeResponse.getPriceRange());
          }
          if (StringUtils.isNotEmpty(priceRangeResponse.getSkuName())) {
            rowItem.setProductName(priceRangeResponse.getSkuName());
          }
        }
      }
    }
  }

  public String generateURL(QrCodeRowItemInfo qrCodeRowItemInfo, QrCodeRowInfo qrCodeRowInfo) {
    String resultUrl;
    if (AllowedQRGenerationType.STORE.getValue().equals(qrCodeRowInfo.getQrGenerationType())
        || AllowedQRGenerationType.ALL_PRODUCTS.getValue()
        .equals(qrCodeRowInfo.getQrGenerationType())) {
      String urlMerchant = qrCodeProperties.getPrefixUrlMerchant();
      urlMerchant = urlMerchant.replace(MERCHANT_CODE, qrCodeRowInfo.getMerchantCode());
      if (StringUtils.isNotEmpty(qrCodeRowItemInfo.getPickupPointCode())) {
        urlMerchant += String.format(PICKUP_POINT_QUERY, qrCodeRowItemInfo.getPickupPointCode());
      }
      resultUrl = urlMerchant;
    } else if (AllowedQRGenerationType.ADD_TO_BAG.getValue()
        .equals(qrCodeRowInfo.getQrGenerationType())) {
      resultUrl = new StringBuilder(qrCodeProperties.getPrefixUrlAddToBag()).append(
              String.format(ITEM_SKU_QUERY, qrCodeRowItemInfo.getItemSku()))
          .append(String.format(PICKUP_POINT_QUERY, qrCodeRowItemInfo.getPickupPointCode())).toString();
    } else {
      String urlProduct = qrCodeProperties.getPrefixUrlProduct();
      String sku =
          AllowedQRGenerationType.PRODUCT.getValue().equals(qrCodeRowInfo.getQrGenerationType()) ?
              PS + qrCodeRowItemInfo.getProductSku() : IS + qrCodeRowItemInfo.getItemSku();
      urlProduct = urlProduct.replace(PRODUCT_SKU, sku);
      if (AllowedQRGenerationType.ITEM_PICKUP_POINT.getValue()
          .equals(qrCodeRowInfo.getQrGenerationType()) && StringUtils.isNotEmpty(
          qrCodeRowItemInfo.getPickupPointCode())) {
        urlProduct += String.format(PICKUP_POINT_QUERY, qrCodeRowItemInfo.getPickupPointCode());
      }
      if (qrCodeRowItemInfo.isCncActivated()) {
        urlProduct += CNC_QUERY;
      }
      resultUrl = urlProduct;
    }
    return generateShortUrl(qrCodeRowInfo.getQrGenerationType(), resultUrl);
  }

  private String generateShortUrl(String qrGenerationType, String scanAndGoUrl) {
    String finalUrl;
    if (urlShortenerSwitchMap.getOrDefault(qrGenerationType, false)) {
      try {
        String shortUrl = cmsOutboundService.generateShortUrl();
        cmsOutboundService.mapLongAndShortUrl(shortUrl, scanAndGoUrl);
        finalUrl = finalShortUrlPrefix + shortUrl;
      } catch (ApplicationRuntimeException e) {
        finalUrl = scanAndGoUrl;
      }
    } else {
      finalUrl = scanAndGoUrl;
    }
    return finalUrl;
  }

  private Tuple<Integer, Integer> getQrSize(Integer qrPerPage , String templateSize) {
    if (StringUtils.equals(templateSize, Constant.SMALL_LABEL) || StringUtils.equals(templateSize, Constant.TRIANGLE_ACRYLIC)) {
      return Tuple.of(800, 800);
    }
    if (qrPerPage == 1) {
      return Tuple.of(800, 800);
    } else if (qrPerPage <= 4) {
      return Tuple.of(400, 400);
    } else if (qrPerPage <= 18) {
      return Tuple.of(256, 256);
    } else {
      return Tuple.of(192, 192);
    }
  }

  private void generatePdfFromHtml(String html, File name) throws IOException {
    Document doc = createWellFormedHtml(html);
    try (OutputStream outputStream = new FileOutputStream(name)) {
      PdfRendererBuilder builder = new PdfRendererBuilder();
      builder.withUri(name.getAbsolutePath());
      builder.toStream(outputStream);
      builder.withW3cDocument(new W3CDom().fromJsoup(doc), File.separator);
      builder.run();
    }
  }

  private Document createWellFormedHtml(String inputHTML) {
    Document document = Jsoup.parse(inputHTML);
    document.outputSettings().syntax(Document.OutputSettings.Syntax.html);
    return document;
  }

  private String qrGenerator(int height, int width, String url, Integer qrPerPage)
      throws Exception {
    log.info("Creating the QR image for path: {}", url);
    String[] rgb = qrcodeColorRgb.split(Constant.COMMA);
    //Blibli QR color
    int RGB =
        new Color(Integer.parseInt(rgb[0]), Integer.parseInt(rgb[1]), Integer.parseInt(rgb[2]),
            255).getRGB();
    Map hintMap = new HashMap();
    hintMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.H);
    hintMap.put(EncodeHintType.MARGIN, 0);
    BitMatrix qrCode =
        new MultiFormatWriter().encode(url, BarcodeFormat.QR_CODE, width, height, hintMap);
    MatrixToImageConfig matrixToImageConfig = new MatrixToImageConfig(RGB, -1);
    BufferedImage qrCodeImage = MatrixToImageWriter.toBufferedImage(qrCode, matrixToImageConfig);
    BufferedImage finalImage = new BufferedImage(qrCodeImage.getWidth(), qrCodeImage.getHeight(),
        BufferedImage.TYPE_INT_ARGB);
    Graphics2D graphics = (Graphics2D) finalImage.getGraphics();
    graphics.drawImage(qrCodeImage, 0, 0, null);
    String logo;
    if (qrPerPage <= 4) {
      logo = qrCodeProperties.getBlibliPinBig();
    } else {
      logo = qrCodeProperties.getBlibliPinSmall();
    }
    log.info("Trying to access the BliBli pin image at: {}", logo);
    byte[] logoImageByte = fileStorageService.downloadFileFromGcs(logo);
    BufferedImage logoImage = ImageIO.read(new ByteArrayInputStream(logoImageByte));
    graphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1f));
    graphics.drawImage(logoImage,
        Math.round((qrCodeImage.getWidth() / 2f) - ((qrCodeImage.getWidth() * 0.1f))),
        Math.round((qrCodeImage.getHeight() / 2f) - ((qrCodeImage.getHeight() * 0.1f))),
        Math.round(qrCodeImage.getWidth() * 0.2f), Math.round(qrCodeImage.getWidth() * 0.2f), null);
    log.info("Saving the QR image at path : {}", url);
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    ImageIO.write(finalImage, PNG, bos);
    return Base64.getEncoder().encodeToString(bos.toByteArray()); // base64 encode
  }
}
