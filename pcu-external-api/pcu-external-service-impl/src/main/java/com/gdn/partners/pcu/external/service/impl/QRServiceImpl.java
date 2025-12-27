package com.gdn.partners.pcu.external.service.impl;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextAttribute;
import java.awt.font.TextLayout;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.properties.QRCodeProperties;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.QRService;
import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;
import com.gdn.partners.pcu.external.web.model.request.QRDownloadWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QRGenerateRequest;
import com.gdn.partners.pcu.external.web.model.request.QRItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QRProductWebRequest;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.MultiFormatWriter;
import com.google.zxing.WriterException;
import com.google.zxing.client.j2se.MatrixToImageConfig;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

import lombok.extern.slf4j.Slf4j;


@Slf4j
@Service
public class QRServiceImpl implements QRService {

  @Autowired
  private QRCodeProperties qrCodeProperties;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private ProductService productService;

  private static final String DELIMITER = "%2F";
  private static final String PS = "ps--";
  private static final String IS = "is--";
  private static final String QR = "_QR";
  private static final String TEMPLATE = "template";
  private static final String DOT = ".";
  private static final String UNDERSCORE = "_";
  private static final String DASH = "-";
  private static final String SLASH = "/";
  private static final String COLON = ":";
  private static final String WHITESPACE = "[\" \"]";
  private static final String PNG = "png";
  private static final String DARK = "Dark";
  private static final String LIGHT = "Light";
  private static final String HEIGHT = "HEIGHT";
  private static final String WIDTH = "WIDTH";
  private static final String xPOSITION = "xPOSITION";
  private static final String yPOSITION = "yPOSITION";
  private static final String TEXT_xPOSITION = "TEXT_xPOSITION";
  private static final String TEXT_yPOSITION = "TEXT_yPOSITION";
  private static final String BREAK_WIDTH = "BREAK_WIDTH";
  private static final String MAX_Y = "MAX_Y";
  private static final String FONT = "FONT";
  private static final String CNC = "CnC";
  private static final String PDF = "pdf";
  private static final String A1_PNG = "A1.png";
  private static final String A5_PNG = "A5.png";
  private static final String A5_CNC_PNG = "A5CnC.png";
  private static final String A1_QR_PNG = "A1_QR.png";
  private static final String A1_CNC_PNG = "A1CnC.png";
  private static final String A1_CNC_QR_PNG = "A1CnC_QR.png";
  private static final String PRODUCT_FOLDER = "products";
  private static final float SCALE = 0.24f;
  private static final String TEMPLATE_FOLDER = "templates";
  private static final String QR_CODES = "QRCodes";
  private static final String ZIP = "zip";
  private static final int MAX_IMAGES_IN_4BY6 = 18;
  private static final int MAX_IMAGES_IN_7BY12 = 4;
  private static final String NEW_Y = "NEW_Y";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_NAME = "merchantName";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PDF_FOLDER = "PDF";
  private static final String ZIP_FOLDER = "ZIP";

  @Override
  public String getPathOfImage(QRGenerateRequest request) throws IOException, WriterException {

    if (Objects.nonNull(request.getProductSkuCode())) {
      return finalPath(productHandler(request));
    } else {
      return finalPath(merchantHandler(request));
    }
  }

  @Override
  public String merchantTemplateDownload(QRGenerateRequest request) throws IOException, WriterException {
    String templateImagePngPath = merchantTemplateGenerator(request);
    PDDocument pdDocument = new PDDocument();
    PDPage blankPage = (TemplateSize.A1.equals(request.getTemplateSize())) ? new PDPage(PDRectangle.A1) :
        new PDPage(PDRectangle.A5);
    pdDocument.addPage(blankPage);
    PDImageXObject pdImage =
        PDImageXObject.createFromFileByExtension(new File(templateImagePngPath).getCanonicalFile(), pdDocument);
    PDPageContentStream contents = new PDPageContentStream(pdDocument, blankPage, PDPageContentStream.AppendMode.APPEND, true);
    contents.drawImage(pdImage, 0, 0, pdImage.getWidth() * SCALE, pdImage.getHeight() * SCALE);
    contents.close();
    StringBuilder fileName = new StringBuilder(templateImagePngPath);
    fileName.replace(fileName.length() - 3, fileName.length(), PDF);
    pdDocument.save(fileName.toString());
    pdDocument.close();
    return finalPath(fileName.toString());
  }

  /**
   * Handler for product template or QR generation
   * @param request
   * @return
   * @throws IOException
   * @throws WriterException
   */
  private String productHandler(QRGenerateRequest request) throws IOException, WriterException {
    if (!Objects.isNull(request.getTemplateSize())) {
      return productTemplateGenerator(request, true);
    } else {
      Map<String, Integer> measurement = getQRSizeAndPosition(request);
      return productQRGenerator(request, measurement, true);
    }
  }

  /**
   * Handler for merchant template or QR generation
   * @param request
   * @return
   * @throws IOException
   * @throws WriterException
   */
  private String merchantHandler(QRGenerateRequest request) throws IOException, WriterException {
    if (!Objects.isNull(request.getTemplateSize())) {
      return merchantTemplateGenerator(request);
    } else {
      Map<String, Integer> measurement = getQRSizeAndPosition(request);
      return merchantQRGenerator(request, measurement);
    }
  }

  /**
   * Get the required template type based on request
   * @param request
   * @return
   */
  private String getTemplateType(QRGenerateRequest request) {
    String requestedTemplateType = TEMPLATE;
    if (Objects.nonNull(request.getIsDarkTheme())) {
      requestedTemplateType = request.getIsDarkTheme() ? requestedTemplateType + DARK : requestedTemplateType + LIGHT;
    }
    if (Objects.nonNull(request.getTemplateSize())) {
      requestedTemplateType = requestedTemplateType + request.getTemplateSize();
    }
    if (Objects.nonNull(request.getIsCnC())) {
      requestedTemplateType = request.getIsCnC() ? requestedTemplateType + CNC : requestedTemplateType;
    }
    return requestedTemplateType;
  }

  /**
   * Generating the QR Image of Store
   * @param request
   * @param measurements
   * @return
   * @throws IOException
   * @throws WriterException
   */
  private String merchantQRGenerator(QRGenerateRequest request, Map<String, Integer> measurements)
      throws IOException, WriterException {
    log.info("Creating the merchant QR Image with merchantName : {}", request.getMerchantName());
    String basePath = qrCodeProperties.getBasePath();
    String templateType = getTemplateType(request);
    StringBuilder merchantContainer = new StringBuilder(basePath + File.separator + request.getMerchantCode());
    File dir = new File(merchantContainer.toString());
    if (!dir.exists()) {
      dir.mkdir();
    }
    StringBuilder newQRPath = new StringBuilder(
        merchantContainer + File.separator + Base64.getEncoder().encodeToString(request.getMerchantName().getBytes())
            + templateType + QR + DOT + PNG);
    log.info("Trying to access the merchant QR file : {}", newQRPath.toString());
    File qRFile = new File(newQRPath.toString());
    if (!qRFile.exists()) {
      log.info("QR doesn't exist, so creating merchant QR file : {}", newQRPath.toString());
      qRGenerator(measurements.get(HEIGHT), measurements.get(WIDTH), generateURL(request, true), newQRPath.toString());
    }
    return newQRPath.toString();
  }

  /**
   * Generating the QR Image of product
   * @param request
   * @param measurements
   * @param isProductLevel
   * @return
   * @throws IOException
   * @throws WriterException
   */
  private String productQRGenerator(QRGenerateRequest request, Map<String, Integer> measurements, boolean isProductLevel)
      throws IOException, WriterException {
    log.info("Creating the product QR Image with productSkuCode : {}", request.getProductSkuCode());
    String basePath = qrCodeProperties.getBasePath();
    String templateType = getTemplateType(request);
    StringBuilder merchantContainer =
        new StringBuilder(basePath + File.separator + request.getMerchantCode());
    File dir = new File(merchantContainer.toString());
    if (!dir.exists()) {
      dir.mkdir();
    }
    StringBuilder productFolder = new StringBuilder(merchantContainer + File.separator + PRODUCT_FOLDER);
    log.info("Creating the products folder at path : {}",productFolder);
    File productDir = new File(productFolder.toString());
    if(!productDir.exists()) {
      productDir.mkdir();
    }
    StringBuilder fileName = new StringBuilder(productFolder + File.separator + Base64.getEncoder().encodeToString(
        new StringBuilder(request.getProductSkuCode()).append(Constants.DASH_SEPARATOR).append(request.getProductSkuName()).toString().getBytes())
        + templateType + QR + DOT + PNG);
    log.info("Trying to access the product QR file : {}", fileName.toString());
    File qRFile = new File(fileName.toString());
    if (!qRFile.exists()) {
      log.info("QR doesn't exist, so creating product QR file : {}", fileName.toString());
      qRGenerator(measurements.get(HEIGHT), measurements.get(WIDTH), generateURL(request, isProductLevel), fileName.toString());
    }
    return fileName.toString();
  }

  /**
   * To generate the merchant template based on request
   * @param request
   * @return
   * @throws IOException
   * @throws WriterException
   */
  private String merchantTemplateGenerator(QRGenerateRequest request) throws IOException, WriterException {
    log.debug("Creating the merchant template of size : {} with request : {}", request.getTemplateSize(), request);
    String basePath = qrCodeProperties.getBasePath();
    String templatePath = qrCodeProperties.getTemplatePath();
    String templateType = getTemplateType(request);
    StringBuilder merchantContainerPath = new StringBuilder(basePath + File.separator + request.getMerchantCode());
    StringBuilder fileName = new StringBuilder(
        Base64.getEncoder().encodeToString(request.getMerchantName().getBytes()) + UNDERSCORE + templateType + DOT
            + PNG);
    //Check the Directory is there or not
    File dir = new File(merchantContainerPath.toString());
    if (!dir.exists()) {
      dir.mkdir();
    }
    StringBuilder newFilePath = new StringBuilder(merchantContainerPath + File.separator + fileName);
    log.info("Trying to access the merchant template file with QR : {}", newFilePath.toString());
    File file = new File(newFilePath.toString());
    if (!file.exists()) {
      log.info("merchant template with QR doesn't exist, so creating merchant template file : {}", newFilePath.toString());
      Map<String, Integer> measurements = getQRSizeAndPosition(request);
      String merchantQRPath = merchantQRGenerator(request, measurements);
      String templateFilePath = templatePath + File.separator + templateType + DOT + PNG;
      String text = request.getMerchantName();
      templateGeneratorWithQRImage(templateFilePath, merchantQRPath, newFilePath.toString(), measurements, text);
    }
    return newFilePath.toString();
  }

  /**
   * To generate the product template on based on request
   * @param request
   * @param isProductLevel
   * @return
   * @throws IOException
   * @throws WriterException
   */
  private String productTemplateGenerator(QRGenerateRequest request, boolean isProductLevel) throws IOException, WriterException {
    log.debug("Creating the product template of size : {} with request : {}", request.getTemplateSize(), request);
    String basePath = qrCodeProperties.getBasePath();
    String templatePath = qrCodeProperties.getTemplatePath();
    String templateType = getTemplateType(request);
    StringBuilder productTemplateContainer =
        new StringBuilder(basePath + File.separator + request.getMerchantCode() + File.separator + PRODUCT_FOLDER);
    File dir = new File(productTemplateContainer.toString());
    if (!dir.exists()) {
      dir.mkdir();
    }
    StringBuilder productTemplateFileName = new StringBuilder(
        productTemplateContainer + File.separator + Base64.getEncoder()
            .encodeToString(new StringBuilder(request.getProductSkuCode()).append(Constants.DASH_SEPARATOR).append(request.getProductSkuName()).toString().getBytes()) + UNDERSCORE + templateType + DOT + PNG);
    log.info("Trying to access the product template file with QR : {}", productTemplateFileName.toString());
    File newFile = new File(productTemplateFileName.toString());
    if (!newFile.exists()) {
      log.info("product template with QR doesn't exist, so creating merchant template file : {}", productTemplateFileName.toString());
      Map<String, Integer> measurements = getQRSizeAndPosition(request);
      String productQRPath = productQRGenerator(request, measurements, isProductLevel);
      String templateFilePath = templatePath + File.separator + templateType + DOT + PNG;
      templateGeneratorWithQRImage(templateFilePath, productQRPath, newFile.toString(), measurements,
          request.getProductSkuName());
    }
    return newFile.toString();
  }

  /**
   * To generate the QR image of specific height and width
   * @param height
   * @param width
   * @param url
   * @param pathName
   * @throws WriterException
   * @throws IOException
   */
  private void qRGenerator(int height, int width, String url, String pathName) throws WriterException, IOException {
    log.info("Creating the QR image for path: {}", pathName);
    int RGB = new Color(15, 90, 132, 255).getRGB();
    Map hintMap = new HashMap();
    hintMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.H);
    hintMap.put(EncodeHintType.MARGIN, 0);
    BitMatrix qrCode = new MultiFormatWriter().encode(url, BarcodeFormat.QR_CODE, width, height, hintMap);
    MatrixToImageConfig matrixToImageConfig = new MatrixToImageConfig(RGB, -1);
    BufferedImage qrCodeImage = MatrixToImageWriter.toBufferedImage(qrCode, matrixToImageConfig);
    BufferedImage finalImage =
        new BufferedImage(qrCodeImage.getWidth(), qrCodeImage.getHeight(), BufferedImage.TYPE_INT_ARGB);
    Graphics2D graphics = (Graphics2D) finalImage.getGraphics();
    graphics.drawImage(qrCodeImage, 0, 0, null);
    String templateFolderPath = qrCodeProperties.getTemplatePath();
    String logo;
    if (pathName.endsWith(A1_PNG) || pathName.endsWith(A1_QR_PNG) || pathName.endsWith(A1_CNC_PNG) || pathName
        .endsWith(A1_CNC_QR_PNG)) {
      logo = qrCodeProperties.getBlibliPinBig();
    } else {
      logo = qrCodeProperties.getBlibliPinSmall();
    }
    StringBuilder pinPath = new StringBuilder(templateFolderPath + File.separator + logo);
    log.info("Trying to access the BliBli pin image at: {}", pinPath);
    BufferedImage logoImage = ImageIO.read(new File(pinPath.toString()));
    graphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1f));
    graphics.drawImage(logoImage, (qrCodeImage.getWidth() - logoImage.getWidth()) / 2,
        (qrCodeImage.getHeight() - logoImage.getHeight()) / 2, null);
    log.info("Saving the QR image at path : {}", pathName);
    ImageIO.write(finalImage, PNG, new File(pathName));
  }

  /**
   * Overlaying the QR image on the required template
   * @param templateFilePath
   * @param qrPathName
   * @param newFilePath
   * @param measurements
   * @param text
   * @throws IOException
   */
  private void templateGeneratorWithQRImage(String templateFilePath, String qrPathName, String newFilePath,
      Map<String, Integer> measurements, String text) throws IOException {
    log.info("Creating the template at path : {}", newFilePath);
    BufferedImage frameImage = ImageIO.read(new File(templateFilePath));
    BufferedImage qrImage = ImageIO.read(new File(qrPathName));
    BufferedImage finalImage =
        new BufferedImage(frameImage.getWidth(), frameImage.getHeight(), BufferedImage.TYPE_INT_ARGB);
    Graphics2D graphics2D = (Graphics2D) finalImage.getGraphics();
    graphics2D.drawImage(frameImage, 0, 0, null);
    graphics2D.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1f));
    graphics2D.drawImage(qrImage, measurements.get(xPOSITION), measurements.get(yPOSITION), null);
    //writing the text on template
    Map<TextAttribute, Object> textAttributeObjectMap = new HashMap<>();
    textAttributeObjectMap.put(TextAttribute.FAMILY, "Effra");
    textAttributeObjectMap.put(TextAttribute.SIZE, new Float(measurements.get(FONT)));

    if (templateFilePath.endsWith(A5_CNC_PNG) || templateFilePath.endsWith(A5_PNG) || templateFilePath.endsWith(A1_PNG)
        || templateFilePath.endsWith(A1_CNC_PNG)) {
      textAttributeObjectMap.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
    }
    log.info("Writing the text on the template at path : {}", newFilePath);
    if (!StringUtils.isBlank(text)) {
      drawText(graphics2D, text, measurements, textAttributeObjectMap);
    }
    ImageIO.write(finalImage, PNG, new File(newFilePath));
  }

  /**
   * To write the text on generated template
   * @param graphics2D
   * @param text
   * @param measurements
   * @param textAttributeObjectMap
   */
  private void drawText(Graphics2D graphics2D, String text, Map<String, Integer> measurements,
      Map<TextAttribute, Object> textAttributeObjectMap) {
    float breakWidth = measurements.get(BREAK_WIDTH);
    int xPosition = measurements.get(TEXT_xPOSITION) ;
    int yPosition = measurements.get(TEXT_yPOSITION) ;
    int maxY = measurements.get(MAX_Y);
    AttributedString attributedString = new AttributedString(text, textAttributeObjectMap);
    graphics2D.setPaint(new Color(2, 65, 91, 255));
    AttributedCharacterIterator attributedCharacterIterator = attributedString.getIterator();
    graphics2D.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HRGB);
    int beginIndex = attributedCharacterIterator.getBeginIndex();
    int endIndex = attributedCharacterIterator.getEndIndex();
    FontRenderContext fRC = graphics2D.getFontRenderContext();
    LineBreakMeasurer lineBreakMeasurer = new LineBreakMeasurer(attributedCharacterIterator, fRC);
    float drawPosY = yPosition;
    lineBreakMeasurer.setPosition(beginIndex);
    while (lineBreakMeasurer.getPosition() < endIndex) {
      if(textAttributeObjectMap.containsKey(TextAttribute.WEIGHT)) {
        int newY = measurements.get(NEW_Y);
        int stringPixels = graphics2D.getFontMetrics(Font.getFont(textAttributeObjectMap)).stringWidth(text);
        if (stringPixels < breakWidth) {
          xPosition = (int) (xPosition + (breakWidth - stringPixels) / 2);
          drawPosY += newY;
        }
      }
      TextLayout layout = lineBreakMeasurer.nextLayout(breakWidth);
      drawPosY += layout.getAscent();
      layout.draw(graphics2D, xPosition, drawPosY);
      drawPosY += layout.getDescent() + layout.getLeading();
      if (drawPosY > maxY) {
        break;
      }
    }
  }

  /**
   * To get the QR size and position for the specific templates
   * @param request
   * @return
   */
  private Map<String, Integer> getQRSizeAndPosition(QRGenerateRequest request) {
    Map<String, Integer> measurements = new HashMap<>();
    if (TemplateSize.A1.equals(request.getTemplateSize())) {
      measurements.put(HEIGHT, 2000);
      measurements.put(WIDTH, 2000);
      measurements.put(xPOSITION, 2500);
      measurements.put(yPOSITION, 4700);
      measurements.put(TEXT_xPOSITION, 1800);
      measurements.put(TEXT_yPOSITION, 3750);
      measurements.put(FONT, 150);
      measurements.put(BREAK_WIDTH, 3386);
      measurements.put(MAX_Y, 4260);
      measurements.put(NEW_Y, 150);
    } else if (TemplateSize.A5.equals(request.getTemplateSize())) {
      measurements.put(HEIGHT, 600);
      measurements.put(WIDTH, 600);
      measurements.put(xPOSITION, 580);
      measurements.put(yPOSITION, 1190);
      measurements.put(TEXT_xPOSITION, 430);
      measurements.put(TEXT_yPOSITION, 880);
      measurements.put(FONT, 60);
      measurements.put(BREAK_WIDTH, 920);
      measurements.put(MAX_Y, 1000);
      measurements.put(NEW_Y, 40);
    } else if (TemplateSize.BY46.equals(request.getTemplateSize())) {
      measurements.put(HEIGHT, 259);
      measurements.put(WIDTH, 259);
      measurements.put(xPOSITION, 27);
      measurements.put(yPOSITION, 190);
      measurements.put(TEXT_xPOSITION, 320);
      measurements.put(TEXT_yPOSITION, 180);
      measurements.put(FONT, 30);
      measurements.put(BREAK_WIDTH, 370);
      measurements.put(MAX_Y, 450);
    } else if (TemplateSize.BY712.equals(request.getTemplateSize())) {
      measurements.put(HEIGHT, 450);
      measurements.put(WIDTH, 450);
      measurements.put(xPOSITION, 95);
      measurements.put(yPOSITION, 325);
      measurements.put(TEXT_xPOSITION, 590);
      measurements.put(TEXT_yPOSITION, 350);
      measurements.put(FONT, 50);
      measurements.put(BREAK_WIDTH, 800);
      measurements.put(MAX_Y, 1000);
    } else {
      // for only QR, default size and no positions required in this case
      measurements.put(HEIGHT, 500);
      measurements.put(WIDTH, 500);
      measurements.put(xPOSITION, 0);
      measurements.put(yPOSITION, 0);
      measurements.put(TEXT_xPOSITION, 0);
      measurements.put(TEXT_yPOSITION, 0);
      measurements.put(FONT, 0);
      measurements.put(BREAK_WIDTH, 0);
      measurements.put(MAX_Y, 0);
    }
    return measurements;
  }

  public String generateURL(QRGenerateRequest qrGenerateRequest, boolean isProductLevel) {
    if (StringUtils.isEmpty(qrGenerateRequest.getProductSkuCode())) {
      String url_merchant = qrCodeProperties.getPrefixUrlMerchant();
      url_merchant = url_merchant.replace(MERCHANT_CODE, qrGenerateRequest.getMerchantCode());
      url_merchant = url_merchant.replace(MERCHANT_NAME, generateUrlName(qrGenerateRequest.getMerchantName()));
      return url_merchant;
    } else {
      String url_product = qrCodeProperties.getPrefixUrlProduct();
      String initiator = (isProductLevel) ? PS : IS;
      url_product = url_product.replace(PRODUCT_SKU, initiator + qrGenerateRequest.getProductSkuCode());
      return url_product;
    }
  }

  /**
   * API to remove all the special character from the name and replace it with '-'.
   *
   * @param name
   */
  private String generateUrlName(String name) {
    name = name.replaceAll("[^A-Za-z0-9]", DASH).toLowerCase();
    StringBuffer generateName = new StringBuffer();
    generateName.append(name.charAt(0));
    for (int count = 1; count < name.length(); count++) {
      if (name.charAt(count) == name.charAt(count - 1) && name.charAt(count) == '-') {
        continue;
      }
      generateName.append(name.charAt(count));
    }
    name = generateName.toString();
    if (name.endsWith(DASH)) {
      name = name.substring(0, name.length() - 1);
    }
    log.info("Generated Name : " + name);
    return name;
  }

  private String finalPath(String path){
    StringBuilder result = new StringBuilder(path);
    int index = path.indexOf(SLASH);
    result.delete(index, index +1);
    int index2 = result.indexOf(SLASH);
    result.delete(0, path.length() - (path.length() - index2));
    return result.toString();
  }

  @Async
  @Override
  public void deleteQRCodes(int days) throws Exception {
    File baseDirectory = new File(qrCodeProperties.getBasePath());
    String[] directories = baseDirectory.list();
    for (String directory : directories) {
      if (!TEMPLATE_FOLDER.equals(directory)) {
        File file = new File(baseDirectory + "/" + directory);
        Date date = new Date(file.lastModified());
        Date allowedDate = DateUtils.addDays(new Date(), -days);
        if (date.compareTo(allowedDate) < 0) {
          Files.walk(Paths.get(file.getPath()), FileVisitOption.FOLLOW_LINKS).sorted(Comparator.reverseOrder())
              .map(Path::toFile).forEach(File::delete);
        }
      }
    }
  }

  @Async
  @Override
  public void downloadQRCodesForProducts(QRDownloadWebRequest request) throws Exception {
    log.info("Creating the QR codes for the request : {}", request);
    Map<String, String> generatedQRCodePath = getGeneratedQRCodesPath(request);
    String zipFilePath = null;
    if (PNG.equals(request.getType())) {
      zipFilePath = zipFiles(generatedQRCodePath, request.getMerchantCode());
    } else {
      String pdfFileName = null;
      if (TemplateSize.BY46.equals(request.getTemplateSize())) {
        pdfFileName = create4By6Pdf(new ArrayList(generatedQRCodePath.values()), request.getMerchantCode());
      } else if (TemplateSize.BY712.equals(request.getTemplateSize())) {
        pdfFileName = create7By12Pdf(new ArrayList(generatedQRCodePath.values()), request.getMerchantCode());
      }
      zipFilePath = zipPDFFile(pdfFileName, request.getMerchantCode());
    }
    log.info("Generated the zip file for the QR codes : {}", zipFilePath);
    pbpFeign.generateQRCodeNotification(request.getMerchantCode(), zipFilePath);
  }

  /**
   * API to retrieve the list of generated QR code path for the products.
   *
   * @param request
   */
  private Map<String, String> getGeneratedQRCodesPath(QRDownloadWebRequest request) throws Exception {
    Map<String, String> generatedQRCodePath = new HashMap<>();
    if (PDF.equals(request.getType())) {
      for (QRProductWebRequest product : request.getProducts()) {
        QRGenerateRequest qrGenerateRequest = new QRGenerateRequest();
        BeanUtils.copyProperties(request, qrGenerateRequest);
        if (request.isProductLevel()) {
          if (StringUtils.isBlank(product.getProductName()))
            continue;
          qrGenerateRequest.setProductSkuCode(product.getProductSku());
          qrGenerateRequest.setProductSkuName(product.getProductName());
          generatedQRCodePath.put(product.getProductSku(), productTemplateGenerator(qrGenerateRequest, true));
        } else {
          Map<String, String> itemNameMap = productService.fetchItemNamesByItemSku(
              product.getItems().stream().map(QRItemWebRequest::getItemSku).distinct().collect(Collectors.toList()));
          for (QRItemWebRequest item : product.getItems()) {
            String itemName = itemNameMap.get(item.getItemSku());
            if (StringUtils.isBlank(itemName))
              continue;
            qrGenerateRequest.setProductSkuName(itemName);
            qrGenerateRequest.setProductSkuCode(item.getItemSku());
            generatedQRCodePath.put(item.getItemSku(), productTemplateGenerator(qrGenerateRequest, false));
          }
        }
      }
    } else {
      for (QRProductWebRequest product : request.getProducts()) {
        QRGenerateRequest qrGenerateRequest = new QRGenerateRequest();
        BeanUtils.copyProperties(request, qrGenerateRequest);
        if (request.isProductLevel()) {
          if (StringUtils.isBlank(product.getProductName()))
            continue;
          qrGenerateRequest.setProductSkuCode(product.getProductSku());
          qrGenerateRequest.setProductSkuName(product.getProductName());
          Map<String, Integer> measurements = getQRSizeAndPosition(qrGenerateRequest);
          String generatedName = product.getProductName().replace(SLASH,DASH).replace(DOT,DASH).replace(COLON,DASH);
          generatedQRCodePath.put(generatedName, productQRGenerator(qrGenerateRequest, measurements, true));
        } else {
          Map<String, String> itemNameMap = productService.fetchItemNamesByItemSku(
              product.getItems().stream().map(QRItemWebRequest::getItemSku).distinct().collect(Collectors.toList()));
          for (QRItemWebRequest item : product.getItems()) {
            String itemName = itemNameMap.get(item.getItemSku());
            if (StringUtils.isBlank(itemName))
              continue;
            qrGenerateRequest.setProductSkuName(itemName);
            qrGenerateRequest.setProductSkuCode(item.getItemSku());
            Map<String, Integer> measurements = getQRSizeAndPosition(qrGenerateRequest);
            String generatedName = itemName.replace(SLASH, DASH).replace(DOT, DASH).replace(COLON, DASH);
            generatedQRCodePath.put(generatedName, productQRGenerator(qrGenerateRequest, measurements, false));
          }
        }
      }
    }
    log.info("Created {} QR codes for the request", generatedQRCodePath.size());
    return generatedQRCodePath;
  }

  /**
   * API to create the pdf document for 4BY6 template QR codes for products
   *
   * @param generatedQRCodePaths
   * @param merchantCode
   */
  private String create4By6Pdf(List<String> generatedQRCodePaths, String merchantCode) throws Exception {
    log.info("Creating the pdf of type 4BY6 for {} QR codes for merchant : {}", generatedQRCodePaths.size(), merchantCode);
    PDDocument document = new PDDocument();
    int pages = generatedQRCodePaths.size() / MAX_IMAGES_IN_4BY6;
    int pageCount = 0;
    while (pageCount <= pages) {
      PDPage page = new PDPage(PDRectangle.A4);
      document.addPage(page);
      PDPageContentStream contentStream = new PDPageContentStream(document, page, true, true);
      int imageCount = 0;
      int row = 0;
      int column = 0;
      while (imageCount < MAX_IMAGES_IN_4BY6) {
        if (pageCount * MAX_IMAGES_IN_4BY6 + imageCount >= generatedQRCodePaths.size()) {
          break;
        }
        PDImageXObject image = PDImageXObject
            .createFromFile(generatedQRCodePaths.get(pageCount * MAX_IMAGES_IN_4BY6 + imageCount), document);
        int xPosition = 0;
        int yPosition = 0;
        if (column == 0) {
          xPosition = 30;
        } else if (column == 1) {
          xPosition = 215;
        } else if (column == 2) {
          xPosition = 400;
        }

        if (row == 0) {
          yPosition = 690;
        } else if (row == 1) {
          yPosition = 560;
        } else if (row == 2) {
          yPosition = 430;
        } else if (row == 3) {
          yPosition = 300;
        } else if (row == 4) {
          yPosition = 170;
        } else if (row == 5) {
          yPosition = 40;
        }
        contentStream.drawImage(image, xPosition, yPosition, image.getWidth() * SCALE, image.getHeight() * SCALE);
        imageCount++;
        column++;
        if (column == 3) {
          column = 0;
          row++;
        }
      }
      contentStream.close();
      pageCount++;
    }
    log.info("Created the pdf of type 4By6 for {} QR codes ", generatedQRCodePaths.size());
    return createProductsPdf(document, merchantCode);
  }

  /**
   * API to create the pdf document for 7By12 template QR codes for products
   *
   * @param generatedQRCodePaths
   * @param merchantCode
   */
  private String create7By12Pdf(List<String> generatedQRCodePaths, String merchantCode) throws Exception {
    log.info("Creating the pdf of type 7BY12 for {} QR codes for merchant : {}", generatedQRCodePaths.size(), merchantCode);
    PDDocument document = new PDDocument();
    int pages = generatedQRCodePaths.size() / MAX_IMAGES_IN_7BY12;
    int pageCount = 0;
    while (pageCount <= pages) {
      PDPage page = new PDPage(new PDRectangle(841.8898F, 595.27563F));
      page.setRotation(90);
      document.addPage(page);
      PDPageContentStream contentStream =
          new PDPageContentStream(document, page, PDPageContentStream.AppendMode.APPEND, true, false);
      int imageCount = 0;
      int row = 0;
      int column = 0;
      while (imageCount < MAX_IMAGES_IN_7BY12) {
        if (pageCount * MAX_IMAGES_IN_7BY12 + imageCount >= generatedQRCodePaths.size()) {
          break;
        }
        PDImageXObject image = PDImageXObject
            .createFromFile(generatedQRCodePaths.get(pageCount * MAX_IMAGES_IN_7BY12 + imageCount), document);

        int xPosition = 0;
        int yPosition = 0;

        if (column == 0) {
          xPosition = 60;
        } else if (column == 1) {
          xPosition = 430;
        }

        if (row == 0) {
          yPosition = 340;
        } else if (row == 1) {
          yPosition = 80;
        }

        contentStream.drawImage(image, xPosition, yPosition, image.getWidth() * SCALE, image.getHeight() * SCALE);
        column++;
        if (column == 2) {
          row++;
          column = 0;
        }
        imageCount++;
      }
      contentStream.close();
      pageCount++;
    }
    log.info("Created the pdf of type 7BY12 for {} QR codes ", generatedQRCodePaths.size());
    return createProductsPdf(document, merchantCode);
  }

  /**
   * API to create the pdf file for the generated document
   *
   * @param document
   * @param merchantCode
   */
  private String createProductsPdf(PDDocument document, String merchantCode) throws IOException {
    String basePath = qrCodeProperties.getBasePath();
    StringBuilder merchantContainer = new StringBuilder(basePath).append(File.separator).append(merchantCode);
    File dir = new File(merchantContainer.toString());
    if (!dir.exists()) {
      dir.mkdir();
    }

    StringBuilder pdfFolder = new StringBuilder(merchantContainer).append(File.separator).append(PDF_FOLDER);
    File file = new File(pdfFolder.toString());
    if (!file.exists()) {
      file.mkdir();
    }

    int count = file.listFiles().length;
    StringBuilder fileName =
        new StringBuilder(merchantContainer).append(File.separator).append(PDF_FOLDER).append(File.separator)
            .append(QR_CODES).append(count + 1).append(DOT).append(PDF);
    document.save(fileName.toString());
    document.close();
    return fileName.toString();
  }

  /**
   * API to create the png files into a zipped folder
   *
   * @param filePaths
   * @param merchantCode
   */
  private String zipFiles(Map<String, String> filePaths, String merchantCode) throws IOException {
    log.info("Creating the zip folder for {} QR codes for merchant : {}", filePaths.size(), merchantCode);
    String basePath = qrCodeProperties.getBasePath();
    StringBuilder merchantContainer = new StringBuilder(basePath + File.separator + merchantCode);
    copyFilesToFolder(filePaths, merchantCode);
    File zipFolder = new File(merchantContainer.toString() + File.separator + ZIP_FOLDER);
    if (!zipFolder.exists()) {
      zipFolder.mkdir();
    }
    int count = zipFolder.listFiles().length;
    String folderPath = basePath + File.separator + merchantCode + File.separator + QR_CODES;
    File directory = new File(folderPath);
    FileOutputStream fos = null;
    ZipOutputStream zos = null;
    try {
      fos = new FileOutputStream(zipFolder + File.separator + QR_CODES + (count + 1) + DOT + ZIP);
      zos = new ZipOutputStream(fos);
      for (File file : directory.listFiles()) {
        ZipEntry zipEntry = new ZipEntry(file.getName());
        zos.putNextEntry(zipEntry);
        byte[] bytes = Files.readAllBytes(Paths.get(file.getPath()));
        zos.write(bytes, 0, bytes.length);
        zos.closeEntry();
      }
    } finally {
      zos.close();
    }
    log.info("Created the zip folder at location : {}", merchantContainer + File.separator + QR_CODES + DOT + ZIP);
    return finalPath(
        merchantContainer + File.separator + ZIP_FOLDER + File.separator + QR_CODES + (count + 1) + DOT + ZIP);
  }

  /**
   * API to copy the file to the QR code folder
   *
   * @param filePaths
   * @param merchantCode
   */
  private void copyFilesToFolder(Map<String, String> filePaths, String merchantCode) throws IOException {
    String basePath = qrCodeProperties.getBasePath();
    File QRCodeDirectory = new File(basePath + File.separator + merchantCode + File.separator + QR_CODES);
    if (QRCodeDirectory.exists()) {
      Files.walk(Paths.get(QRCodeDirectory.getPath()), FileVisitOption.FOLLOW_LINKS).sorted(Comparator.reverseOrder())
          .map(Path::toFile).forEach(File::delete);
    }
    QRCodeDirectory.mkdir();
    String destinationPath = basePath + File.separator + merchantCode + File.separator + QR_CODES + File.separator;
    for (Map.Entry entry : filePaths.entrySet()) {
      File sourceFile = new File(entry.getValue().toString());
      File destinationFile = new File(destinationPath + entry.getKey() + DOT + PNG);
      FileUtils.copyFile(sourceFile, destinationFile);
    }
    log.info("copied the required QR codes to the folder : {}", QRCodeDirectory.getPath());
  }

  /**
   * API to zip the pdf file
   *
   * @param path
   * @param merchantCode
   */
  private String zipPDFFile(String path, String merchantCode) throws Exception {
    log.info("Creating the zip file for the generated pdf at location {} for merchant : {}", path, merchantCode);
    FileOutputStream fos = null;
    ZipOutputStream zos = null;
    String generatedFileName = null;
    String zipFileName = null;
    StringBuilder merchantContainer = new StringBuilder(qrCodeProperties.getBasePath() + File.separator + merchantCode);
    try {
      File file = new File(path);
      zipFileName = file.getName().concat(".zip");
      generatedFileName = merchantContainer + File.separator + zipFileName;
      fos = new FileOutputStream(generatedFileName);
      zos = new ZipOutputStream(fos);
      zos.putNextEntry(new ZipEntry(file.getName()));
      byte[] bytes = Files.readAllBytes(Paths.get(path));
      zos.write(bytes, 0, bytes.length);
    } finally {
      zos.closeEntry();
      zos.close();
    }
    log.info("Created the zip file for pdf at location {}", generatedFileName);
    return finalPath(generatedFileName);
  }
}
