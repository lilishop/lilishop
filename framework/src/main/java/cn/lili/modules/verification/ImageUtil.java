package cn.lili.modules.verification;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.IOException;

/**
 * ImageUtil
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-17 14:50
 */
public class ImageUtil {

    /**
     * 添加水印
     *
     * @param oriImage 原图
     * @param text     文字
     * @throws IOException 流操作异常
     */
    public static void addWatermark(BufferedImage oriImage, String text) {
        Graphics2D graphics2D = oriImage.createGraphics();
        //设置水印文字颜色
        graphics2D.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        //设置水印文字Font
        graphics2D.setColor(Color.black);
        //设置水印文字透明度
        graphics2D.setFont(new Font("宋体", Font.BOLD, 30));
        //第一参数->设置的内容，后面两个参数->文字在图片上的坐标位置(x,y)
        graphics2D.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_ATOP, 0.4f));
        graphics2D.drawString(text, 10, 40);
        graphics2D.dispose();
    }


    /**
     * 干扰图
     *
     * @param oriImage      原图
     * @param templateImage 模板图
     * @param x             随机扣取坐标X
     * @param y             随机扣取坐标y
     */
    public static void interfereTemplate(BufferedImage oriImage, BufferedImage templateImage,
                                         int x, int y) {
        //临时数组遍历用于高斯模糊存周边像素值
        int[][] matrix = new int[3][3];
        int[] values = new int[9];

        int xLength = templateImage.getWidth();
        int yLength = templateImage.getHeight();
        //模板图像宽度
        for (int i = 0; i < xLength; i++) {
            //模板图片高度
            for (int j = 0; j < yLength; j++) {
                //如果模板图像当前像素点不是透明色 copy源文件信息到目标图片中
                int rgb = templateImage.getRGB(i, j);
                if (rgb < 0) {
                    //抠图区域高斯模糊
                    readPixel(oriImage, x + i, y + j, values);
                    fillMatrix(matrix, values);
                    oriImage.setRGB(x + i, y + j, avgMatrix(matrix));
                }

                //防止数组越界判断
                if (i == (xLength - 1) || j == (yLength - 1)) {
                    continue;
                }
                int rightRgb = templateImage.getRGB(i + 1, j);
                int downRgb = templateImage.getRGB(i, j + 1);
                //描边处理，,取带像素和无像素的界点，判断该点是不是临界轮廓点,如果是设置该坐标像素是白色
                boolean rgbImage = ((rgb >= 0 && rightRgb < 0)
                        || (rgb < 0 && rightRgb >= 0)
                        || (rgb >= 0 && downRgb < 0)
                        || (rgb < 0 && downRgb >= 0));
            }
        }
    }

    /**
     * @param oriImage      原图
     * @param templateImage 模板图
     * @param newImage      新抠出的小图
     * @param x             随机扣取坐标X
     * @param y             随机扣取坐标y
     */
    public static void cutByTemplate(BufferedImage oriImage, BufferedImage templateImage, BufferedImage newImage,
                                     int x, int y) {
        //临时数组遍历用于高斯模糊存周边像素值
        int[][] matrix = new int[3][3];
        int[] values = new int[9];

        int xLength = templateImage.getWidth();
        int yLength = templateImage.getHeight();
        //模板图像宽度
        for (int i = 0; i < xLength; i++) {
            //模板图片高度
            for (int j = 0; j < yLength; j++) {
                //如果模板图像当前像素点不是透明色 copy源文件信息到目标图片中
                int rgb = templateImage.getRGB(i, j);
                if (rgb < 0) {
                    newImage.setRGB(i, j, oriImage.getRGB(x + i, y + j));

                    //抠图区域高斯模糊
                    readPixel(oriImage, x + i, y + j, values);
                    fillMatrix(matrix, values);
                    oriImage.setRGB(x + i, y + j, avgMatrix(matrix));
                }

                //防止数组越界判断
                if (i == (xLength - 1) || j == (yLength - 1)) {
                    continue;
                }
                int rightRgb = templateImage.getRGB(i + 1, j);
                int downRgb = templateImage.getRGB(i, j + 1);
                //描边处理，,取带像素和无像素的界点，判断该点是不是临界轮廓点,如果是设置该坐标像素是白色
                boolean rgbImage = ((rgb >= 0 && rightRgb < 0)
                        || (rgb < 0 && rightRgb >= 0)
                        || (rgb >= 0 && downRgb < 0)
                        || (rgb < 0 && downRgb >= 0));

                if (rgbImage) {
                    newImage.setRGB(i, j, Color.GRAY.getRGB());
                }
            }
        }
    }

    public static void readPixel(BufferedImage img, int x, int y, int[] pixels) {
        int xStart = x - 1;
        int yStart = y - 1;
        int current = 0;
        for (int i = xStart; i < 3 + xStart; i++) {
            for (int j = yStart; j < 3 + yStart; j++) {
                int tx = i;
                if (tx < 0) {
                    tx = -tx;

                } else if (tx >= img.getWidth()) {
                    tx = x;
                }
                int ty = j;
                if (ty < 0) {
                    ty = -ty;
                } else if (ty >= img.getHeight()) {
                    ty = y;
                }
                pixels[current++] = img.getRGB(tx, ty);

            }
        }
    }

    public static void fillMatrix(int[][] matrix, int[] values) {
        int filled = 0;
        for (int[] x : matrix) {
            for (int j = 0; j < x.length; j++) {
                x[j] = values[filled++];
            }
        }
    }

    public static int avgMatrix(int[][] matrix) {
        int r = 0;
        int g = 0;
        int b = 0;
        for (int[] x : matrix) {
            for (int j = 0; j < x.length; j++) {
                if (j == 1) {
                    continue;
                }
                Color c = new Color(x[j]);
                r += c.getRed();
                g += c.getGreen();
                b += c.getBlue();
            }
        }
        return new Color(r / 8, g / 8, b / 8).getRGB();
    }
}
