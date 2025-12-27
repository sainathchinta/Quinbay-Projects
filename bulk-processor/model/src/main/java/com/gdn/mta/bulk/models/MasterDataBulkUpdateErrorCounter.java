package com.gdn.mta.bulk.models;

public class MasterDataBulkUpdateErrorCounter {

  private int productCodeErrorCount;
  private int productNameErrorCount;
  private int lengthErrorCount;
  private int widthErrorCount;
  private int heightErrorCount;
  private int weightErrorCount;
  private int brandErrorCount;
  private int dangerousGoodsLevelErrorCount;
  private int inputErrorCount;
  private int systemErrorCount;

  public MasterDataBulkUpdateErrorCounter() {
  }

  public int getProductCodeErrorCount() {
    return productCodeErrorCount;
  }

  public void setProductCodeErrorCount(int productCodeErrorCount) {
    this.productCodeErrorCount = productCodeErrorCount;
  }

  public int getProductNameErrorCount() {
    return productNameErrorCount;
  }

  public void setProductNameErrorCount(int productNameErrorCount) {
    this.productNameErrorCount = productNameErrorCount;
  }

  public int getLengthErrorCount() {
    return lengthErrorCount;
  }

  public void setLengthErrorCount(int lengthErrorCount) {
    this.lengthErrorCount = lengthErrorCount;
  }

  public int getWidthErrorCount() {
    return widthErrorCount;
  }

  public void setWidthErrorCount(int widthErrorCount) {
    this.widthErrorCount = widthErrorCount;
  }

  public int getHeightErrorCount() {
    return heightErrorCount;
  }

  public void setHeightErrorCount(int heightErrorCount) {
    this.heightErrorCount = heightErrorCount;
  }

  public int getWeightErrorCount() {
    return weightErrorCount;
  }

  public void setWeightErrorCount(int weightErrorCount) {
    this.weightErrorCount = weightErrorCount;
  }

  public int getBrandErrorCount() {
    return brandErrorCount;
  }

  public void setBrandErrorCount(int brandErrorCount) {
    this.brandErrorCount = brandErrorCount;
  }

  public int getDangerousGoodsLevelErrorCount() {
    return dangerousGoodsLevelErrorCount;
  }

  public void setDangerousGoodsLevelErrorCount(int dangerousGoodsLevelErrorCount) {
    this.dangerousGoodsLevelErrorCount = dangerousGoodsLevelErrorCount;
  }

  public int getInputErrorCount() {
    return inputErrorCount;
  }

  public void setInputErrorCount(int inputErrorCount) {
    this.inputErrorCount = inputErrorCount;
  }

  public int getSystemErrorCount() {
    return systemErrorCount;
  }

  public void setSystemErrorCount(int systemErrorCount) {
    this.systemErrorCount = systemErrorCount;
  }

  public void incrementProductCodeErrorCount(){
    setProductCodeErrorCount(getProductCodeErrorCount() + 1);
  }

  public void incrementProductNameErrorCount(){
    setProductNameErrorCount(getProductNameErrorCount() + 1);
  }

  public void incrementLengthErrorCount(){
    setLengthErrorCount(getLengthErrorCount() + 1);
  }

  public void incrementWidthErrorCount(){
    setWidthErrorCount(getWidthErrorCount() + 1);
  }

  public void incrementHeightErrorCount(){
    setHeightErrorCount(getHeightErrorCount() + 1);
  }

  public void incrementWeightErrorCount(){
    setWeightErrorCount(getWeightErrorCount() + 1);
  }

  public void incrementBrandErrorCount(){
    setBrandErrorCount(getBrandErrorCount() + 1);
  }

  public void incrementDangerousGoodsLevelErrorCount(){
    setDangerousGoodsLevelErrorCount(getDangerousGoodsLevelErrorCount() + 1);
  }

  public void incrementInputErrorCount(){
    setInputErrorCount(getInputErrorCount() + 1);
  }

  public void incrementSystemErrorCount(){
    setSystemErrorCount(getSystemErrorCount() + 1);
  }
}
