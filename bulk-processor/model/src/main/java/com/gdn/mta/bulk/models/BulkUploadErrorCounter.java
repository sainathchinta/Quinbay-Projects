package com.gdn.mta.bulk.models;

/**
 * Created by priyanka on 16/02/17.
 */
public class BulkUploadErrorCounter {

  private int productName;
  private int productType;
  private int harga;
  private int hargaPenjualan;
  private int stock;
  private int pickupPoint;
  private int deliveryStatus;
  private int cncActive;
  private int length;
  private int width;
  private int height;
  private int weight;
  private int description;
  private int uniqueSellingPoint;
  private int installation;
  private int display;
  private int buyable;
  private int feature;
  private int variation;
  private int image;
  private int inputErrorCount;
  private int systemError;

  public BulkUploadErrorCounter() {
  }

  public int getProductName() {
    return productName;
  }

  public void setProductName(int productName) {
    this.productName = productName;
  }

  public int getProductType() {
    return productType;
  }

  public void setProductType(int productType) {
    this.productType = productType;
  }

  public int getHarga() {
    return harga;
  }

  public void setHarga(int harga) {
    this.harga = harga;
  }

  public int getHargaPenjualan() {
    return hargaPenjualan;
  }

  public void setHargaPenjualan(int hargaPenjualan) {
    this.hargaPenjualan = hargaPenjualan;
  }

  public int getStock() {
    return stock;
  }

  public void setStock(int stock) {
    this.stock = stock;
  }

  public int getPickupPoint() {
    return pickupPoint;
  }

  public void setPickupPoint(int pickupPoint) {
    this.pickupPoint = pickupPoint;
  }

  public int getDeliveryStatus() {
    return deliveryStatus;
  }

  public void setDeliveryStatus(int deliveryStatus) {
    this.deliveryStatus = deliveryStatus;
  }

  public int getCncActive() {
    return cncActive;
  }

  public void setCncActive(int cncActive) {
    this.cncActive = cncActive;
  }

  public int getLength() {
    return length;
  }

  public void setLength(int length) {
    this.length = length;
  }

  public int getWidth() {
    return width;
  }

  public void setWidth(int width) {
    this.width = width;
  }

  public int getHeight() {
    return height;
  }

  public void setHeight(int height) {
    this.height = height;
  }

  public int getWeight() {
    return weight;
  }

  public void setWeight(int weight) {
    this.weight = weight;
  }

  public int getDescription() {
    return description;
  }

  public void setDescription(int description) {
    this.description = description;
  }

  public int getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public void setUniqueSellingPoint(int uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public int getInstallation() {
    return installation;
  }

  public void setInstallation(int installation) {
    this.installation = installation;
  }

  public int getDisplay() {
    return display;
  }

  public void setDisplay(int display) {
    this.display = display;
  }

  public int getBuyable() {
    return buyable;
  }

  public void setBuyable(int buyable) {
    this.buyable = buyable;
  }

  public int getFeature() {
    return feature;
  }

  public void setFeature(int feature) {
    this.feature = feature;
  }

  public int getVariation() {
    return variation;
  }

  public void setVariation(int variation) {
    this.variation = variation;
  }

  public int getImage() {
    return image;
  }

  public void setImage(int image) {
    this.image = image;
  }

  public int getSystemError() { return systemError; }

  public void setSystemError(int systemError) { this.systemError = systemError; }

  public void incrementProductName(){
    setProductName(1 + getProductName());
  }

  public void incrementProductType(){
    setProductType(1 + getProductType());
  }

  public void incrementHarga(){
    setHarga(1 + getHarga());
  }

  public void incrementHargaPenjualan(){
    setHargaPenjualan(1 + getHargaPenjualan());
  }

  public void incrementStock(){
    setStock(1 + getStock());
  }

  public void incrementPickupPoint(){
    setPickupPoint(1 + getPickupPoint());
  }

  public void incrementDeliveryStatus() {
    setDeliveryStatus(1 + getDeliveryStatus());
  }

  public void incrementCncActive() {
    setCncActive(1 + getCncActive());
  }


  public void incrementLength(){
    setLength(1 + getLength());
  }

  public void incrementWidth(){
    setWidth(1 + getWidth());
  }

  public void incrementHeight(){
    setHeight(1 + getHeight());
  }

  public void incrementWeight(){
    setWeight(1 + getWeight());
  }

  public void incrementDescription(){
    setDescription(1 + getDescription());
  }

  public void incrementInstallation(){
    setInstallation(1 + getInstallation());
  }

  public void incrementDisplay(){
    setDisplay(1 + getDisplay());
  }

  public void incrementBuyable(){
    setBuyable(1 + getBuyable());
  }

  public void incrementFeature(){
    setFeature(1 + getFeature());
  }

  public void incrementVariation(){
    setVariation(1 + getVariation());
  }

  public void incrementImage(){
    setImage(1 + getImage());
  }

  public void incrementUniqueSellingPoint() {
    setUniqueSellingPoint(1 + getUniqueSellingPoint());
  }

  public void incrementSystemError(){
    setSystemError(1 + getSystemError());
  }

  public int getInputErrorCount() {
    return inputErrorCount;
  }

  public void setInputErrorCount(int inputErrorCount) {
    this.inputErrorCount = inputErrorCount;
  }

  public void  incrementInputErrorCount(){
    setInputErrorCount(1 + getInputErrorCount());
  }

}
