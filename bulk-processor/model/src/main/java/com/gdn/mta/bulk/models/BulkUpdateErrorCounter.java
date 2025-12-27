package com.gdn.mta.bulk.models;

/**
 * Created by priyanka on 13/02/17.
 */
public class BulkUpdateErrorCounter {
  private int blibliSkuCounter;
  private int blibliProductSkuCounter;
  private int productNameCounter;
  private int hargaCounter;
  private int hargaPenjualanCounter;
  private int stockCounter;
  private int poQuotaCounter;
  private int cncStatusCounter;
  private int tipePenangananCounter;
  private int pickupPointCounter;
  private int booleanHeaderCounter;
  private int systemErrorCounter;
  private int inputErrorCounter;
  private int sellerSkuCounter;
  private int headerValidationCounter;

  public BulkUpdateErrorCounter() {
  }

  public int getCncStatusCounter() {
    return cncStatusCounter;
  }

  public void setCncStatusCounter(int cncStatusCounter) {
    this.cncStatusCounter = cncStatusCounter;
  }

  public int getBlibliSkuCounter() {
    return blibliSkuCounter;
  }

  public void setBlibliSkuCounter(int blibliSkuCounter) {
    this.blibliSkuCounter = blibliSkuCounter;
  }

  public int getBlibliProductSkuCounter() {
    return blibliProductSkuCounter;
  }

  public void setBlibliProductSkuCounter(int blibliProductSkuCounter) {
    this.blibliProductSkuCounter = blibliProductSkuCounter;
  }

  public int getProductNameCounter() {
    return productNameCounter;
  }

  public void setProductNameCounter(int productNameCounter) {
    this.productNameCounter = productNameCounter;
  }

  public int getHargaCounter() {
    return hargaCounter;
  }

  public void setHargaCounter(int hargaCounter) {
    this.hargaCounter = hargaCounter;
  }

  public int getHargaPenjualanCounter() {
    return hargaPenjualanCounter;
  }

  public void setHargaPenjualanCounter(int hargaPenjualanCounter) {
    this.hargaPenjualanCounter = hargaPenjualanCounter;
  }

  public int getStockCounter() {
    return stockCounter;
  }

  public void setStockCounter(int stockCounter) {
    this.stockCounter = stockCounter;
  }

  public int getPoQuotaCounter() {
    return poQuotaCounter;
  }

  public void setPoQuotaCounter(int poQuotaCounter) {
    this.poQuotaCounter = poQuotaCounter;
  }

  public int getBooleanHeaderCounter() {
    return booleanHeaderCounter;
  }

  public void setBooleanHeaderCounter(int booleanHeaderCounter) {
    this.booleanHeaderCounter = booleanHeaderCounter;
  }

  public int getTipePenangananCounter() {
    return tipePenangananCounter;
  }

  public void setTipePenangananCounter(int tipePenangananCounter) {
    this.tipePenangananCounter = tipePenangananCounter;
  }

  public int getPickupPointCounter() {
    return pickupPointCounter;
  }

  public void setPickupPointCounter(int pickupPointCounter) {
    this.pickupPointCounter = pickupPointCounter;
  }

  public int getSystemErrorCounter() {
    return systemErrorCounter;
  }

  public void setSystemErrorCounter(int systemErrorCounter) {
    this.systemErrorCounter = systemErrorCounter;
  }

  public void incrementCncStatusCounter() {
    setCncStatusCounter(1 + getCncStatusCounter());
  }

  public void incrementBlibliSkuCounter(){
    setBlibliSkuCounter(1 + getBlibliSkuCounter());
  }

  public void incrementBlibliProductSkuCounter() {
    setBlibliProductSkuCounter(1 + getBlibliProductSkuCounter());
  }

  public void incrementProductNameCounter(){
    setProductNameCounter(1 + getProductNameCounter());
  }

  public void incrementHargaCounter(){
    setHargaCounter(1 + getHargaCounter());
  }

  public void incrementHargaPenjualanCounter(){
    setHargaPenjualanCounter(1 + getHargaPenjualanCounter());
  }

  public void incrementStockCounter(){
    setStockCounter(1 + getStockCounter());
  }

  public void incrementPoQuotaCounter(){
    setPoQuotaCounter(1 + getPoQuotaCounter());
  }

  public void incrementTipePenangananCounter(){
    setTipePenangananCounter(1 + getTipePenangananCounter());
  }

  public void incrementPickupPointCounter(){
    setPickupPointCounter(1 + getPickupPointCounter());
  }

  public void incrementBooleanHeader(){
    setBooleanHeaderCounter(1 + getBooleanHeaderCounter());
  }

  public void incrementSystemErrorCounter(){
    setSystemErrorCounter(1 + getSystemErrorCounter());
  }

  public int getInputErrorCounter() {
    return inputErrorCounter;
  }

  public void setInputErrorCounter(int inputErrorCounter) {
    this.inputErrorCounter = inputErrorCounter;
  }

  public void incrementInputErrorCounter(){
    setInputErrorCounter(1 + getInputErrorCounter());
  }

  public int getSellerSkuCounter() {
    return sellerSkuCounter;
  }

  public void setSellerSkuCounter(int sellerSkuCounter) {
    this.sellerSkuCounter = sellerSkuCounter;
  }

  public void incrementSellerSkuCounter() {
    setSellerSkuCounter(1 + getSellerSkuCounter());
  }

  public int getHeaderValidationCounter() {
    return headerValidationCounter;
  }

  public void setHeaderValidationCounter(int headerValidationCounter) {
    this.headerValidationCounter = headerValidationCounter;
  }

  public void incrementHeaderValidationCounter(){
    setHeaderValidationCounter(1 + getHeaderValidationCounter());
  }
}
