package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

@Document(collection = TicketTemplate.DOCUMENT_NAME)
public class TicketTemplate extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  public static final String DOCUMENT_NAME = "prd_ticket_template";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.TICKET_TEMPLATE_CODE)
  private String ticketTemplateCode;

  @Field(value = ProductFieldNames.NAME)
  private String name;

  @Field(value = ProductFieldNames.FROM)
  private String from;

  @Field(value = ProductFieldNames.CC)
  private String cc;

  @Field(value = ProductFieldNames.BCC)
  private String bcc;

  @Field(value = ProductFieldNames.ACTIVE)
  private boolean active = true;

  @Field(value = ProductFieldNames.SUBJECT_ORDER_RECEIVED)
  private String subjectOrderReceived;

  @Field(value = ProductFieldNames.SUBJECT_ORDER_PAID)
  private String subjectOrderPaid;

  @Field(value = ProductFieldNames.SUBJECT_BARCODE)
  private String subjectBarcode;

  @Field(value = ProductFieldNames.TEMPLATE_ORDER_RECEIVED)
  private String templateOrderReceived;

  @Field(value = ProductFieldNames.TEMPLATE_ORDER_PAID)
  private String templateOrderPaid;

  @Field(value = ProductFieldNames.TEMPLATE_BARCODE)
  private String templateBarcode;

  @Field(value = ProductFieldNames.BILLINGUAL)
  private boolean bilingual;

  @Field(value = ProductFieldNames.TEMPLATE_BARCODE_HEADING_TEXT)
  private String templateBarcodeHeadingText;

  @Field(value = ProductFieldNames.TEMPLATE_BARCODE_PROCESS_TITLE)
  private String templateBarcodeProcessTitle;

  @Field(value = ProductFieldNames.TEMPLATE_BARCODE_PROCESS_TEXT)
  private String templateBarcodeProcessText;

  @Field(value = ProductFieldNames.TEMPLATE_ORDER_PROCESS_TITLE)
  private String templateOrderProcessTitle;

  @Field(value = ProductFieldNames.TEMPLATE_ORDER_PROCESS_TEXT1)
  private String templateOrderProcessText1;

  @Field(value = ProductFieldNames.TEMPLATE_ORDER_PROCESS_TEXT2)
  private String templateOrderProcessText2;

  @Field(value = ProductFieldNames.TEMPLATE_PAYMENT_PROCESS_TITLE)
  private String templatePaymentProcessTitle;

  @Field(value = ProductFieldNames.TEMPLATE_PAYMENT_PROCESS_TEXT1)
  private String templatePaymentProcessText1;

  @Field(value = ProductFieldNames.TEMPLATE_PAYMENT_PROCESS_TEXT2)
  private String templatePaymentProcessText2;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBcc() {
    return this.bcc;
  }

  public String getCc() {
    return this.cc;
  }

  public String getFrom() {
    return this.from;
  }

  public String getName() {
    return this.name;
  }

  public String getSubjectBarcode() {
    return this.subjectBarcode;
  }

  public String getSubjectOrderPaid() {
    return this.subjectOrderPaid;
  }

  public String getSubjectOrderReceived() {
    return this.subjectOrderReceived;
  }

  public String getTemplateBarcode() {
    return this.templateBarcode;
  }

  public String getTemplateBarcodeHeadingText() {
    return this.templateBarcodeHeadingText;
  }

  public String getTemplateBarcodeProcessText() {
    return this.templateBarcodeProcessText;
  }

  public String getTemplateBarcodeProcessTitle() {
    return this.templateBarcodeProcessTitle;
  }

  public String getTemplateOrderPaid() {
    return this.templateOrderPaid;
  }

  public String getTemplateOrderProcessText1() {
    return this.templateOrderProcessText1;
  }

  public String getTemplateOrderProcessText2() {
    return this.templateOrderProcessText2;
  }

  public String getTemplateOrderProcessTitle() {
    return this.templateOrderProcessTitle;
  }

  public String getTemplateOrderReceived() {
    return this.templateOrderReceived;
  }

  public String getTemplatePaymentProcessText1() {
    return this.templatePaymentProcessText1;
  }

  public String getTemplatePaymentProcessText2() {
    return this.templatePaymentProcessText2;
  }

  public String getTemplatePaymentProcessTitle() {
    return this.templatePaymentProcessTitle;
  }

  public String getTicketTemplateCode() {
    return this.ticketTemplateCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isActive() {
    return this.active;
  }

  public boolean isBilingual() {
    return this.bilingual;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public void setBcc(String bcc) {
    this.bcc = bcc;
  }

  public void setBilingual(boolean bilingual) {
    this.bilingual = bilingual;
  }

  public void setCc(String cc) {
    this.cc = cc;
  }

  public void setFrom(String from) {
    this.from = from;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setSubjectBarcode(String subjectBarcode) {
    this.subjectBarcode = subjectBarcode;
  }

  public void setSubjectOrderPaid(String subjectOrderPaid) {
    this.subjectOrderPaid = subjectOrderPaid;
  }

  public void setSubjectOrderReceived(String subjectOrderReceived) {
    this.subjectOrderReceived = subjectOrderReceived;
  }

  public void setTemplateBarcode(String templateBarcode) {
    this.templateBarcode = templateBarcode;
  }

  public void setTemplateBarcodeHeadingText(String templateBarcodeHeadingText) {
    this.templateBarcodeHeadingText = templateBarcodeHeadingText;
  }

  public void setTemplateBarcodeProcessText(String templateBarcodeProcessText) {
    this.templateBarcodeProcessText = templateBarcodeProcessText;
  }

  public void setTemplateBarcodeProcessTitle(String templateBarcodeProcessTitle) {
    this.templateBarcodeProcessTitle = templateBarcodeProcessTitle;
  }

  public void setTemplateOrderPaid(String templateOrderPaid) {
    this.templateOrderPaid = templateOrderPaid;
  }

  public void setTemplateOrderProcessText1(String templateOrderProcessText1) {
    this.templateOrderProcessText1 = templateOrderProcessText1;
  }

  public void setTemplateOrderProcessText2(String templateOrderProcessText2) {
    this.templateOrderProcessText2 = templateOrderProcessText2;
  }

  public void setTemplateOrderProcessTitle(String templateOrderProcessTitle) {
    this.templateOrderProcessTitle = templateOrderProcessTitle;
  }

  public void setTemplateOrderReceived(String templateOrderReceived) {
    this.templateOrderReceived = templateOrderReceived;
  }

  public void setTemplatePaymentProcessText1(String templatePaymentProcessText1) {
    this.templatePaymentProcessText1 = templatePaymentProcessText1;
  }

  public void setTemplatePaymentProcessText2(String templatePaymentProcessText2) {
    this.templatePaymentProcessText2 = templatePaymentProcessText2;
  }

  public void setTemplatePaymentProcessTitle(String templatePaymentProcessTitle) {
    this.templatePaymentProcessTitle = templatePaymentProcessTitle;
  }

  public void setTicketTemplateCode(String ticketTemplateCode) {
    this.ticketTemplateCode = ticketTemplateCode;
  }

  @Override
  public String toString() {
    return String
        .format(
            "TicketTemplate [ticketTemplateId=%s, name=%s, from=%s, cc=%s, bcc=%s, active=%s, subjectOrderReceived=%s, subjectOrderPaid=%s, subjectBarcode=%s, templateOrderReceived=%s, templateOrderPaid=%s, templateBarcode=%s, bilingual=%s, templateBarcodeHeadingText=%s, templateBarcodeProcessTitle=%s, templateBarcodeProcessText=%s, templateOrderProcessTitle=%s, templateOrderProcessText1=%s, templateOrderProcessText2=%s, templatePaymentProcessTitle=%s, templatePaymentProcessText1=%s, templatePaymentProcessText2=%s, toString()=%s]",
            this.ticketTemplateCode, this.name, this.from, this.cc, this.bcc, this.active,
            this.subjectOrderReceived, this.subjectOrderPaid, this.subjectBarcode,
            this.templateOrderReceived, this.templateOrderPaid, this.templateBarcode,
            this.bilingual, this.templateBarcodeHeadingText, this.templateBarcodeProcessTitle,
            this.templateBarcodeProcessText, this.templateOrderProcessTitle,
            this.templateOrderProcessText1, this.templateOrderProcessText2,
            this.templatePaymentProcessTitle, this.templatePaymentProcessText1,
            this.templatePaymentProcessText2, super.toString());
  }

}
