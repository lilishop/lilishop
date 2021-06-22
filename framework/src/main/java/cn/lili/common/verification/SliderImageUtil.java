package cn.lili.common.verification;

import cn.lili.common.utils.Base64DecodeMultipartFile;
import cn.lili.common.vo.SerializableStream;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.Base64Utils;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * 验证码工具
 *
 * @author Chopper
 * @version v4.0
 * @Description:
 * @since 2020/11/17 14:34
 */
@Component
@Slf4j
public class SliderImageUtil {

    private static final int BOLD = 5;

    private static final String IMG_FILE_TYPE = "jpg";

    private static final String TEMP_IMG_FILE_TYPE = "png";

    /**
     * 根据模板切图
     *
     * @param sliderFile
     * @param originalFile
     * @return
     * @throws Exception sliderFile, originalFile
     */
    public static Map<String, Object> pictureTemplatesCut(SerializableStream sliderFile, SerializableStream originalFile) throws Exception {

        Random random = new Random();
        Map<String, Object> pictureMap = new HashMap<>();
        //拼图
        BufferedImage sliderImage = ImageIO.read(Base64DecodeMultipartFile.base64ToInputStream(sliderFile.getBase64()));
        int sliderWidth = sliderImage.getWidth();
        int sliderHeight = sliderImage.getHeight();

        //原图
        BufferedImage originalImage = ImageIO.read(Base64DecodeMultipartFile.base64ToInputStream(originalFile.getBase64()));
        int originalWidth = originalImage.getWidth();
        int originalHeight = originalImage.getHeight();

        //随机生成抠图坐标X,Y
        //X轴距离右端targetWidth Y轴距离底部targetHeight以上
        int randomX = random.nextInt(originalWidth - 3 * sliderWidth) + 2 * sliderWidth;
        int randomY = random.nextInt(originalHeight - sliderHeight);
        log.info("原图大小{} x {},随机生成的坐标 X,Y 为（{}，{}）", originalWidth, originalHeight, randomX, randomY);

        //新建一个和模板一样大小的图像，TYPE_4BYTE_ABGR表示具有8位RGBA颜色分量的图像，正常取imageTemplate.getType()
        BufferedImage newImage = new BufferedImage(sliderWidth, sliderHeight, sliderImage.getType());
        //得到画笔对象
        Graphics2D graphics = newImage.createGraphics();
        //如果需要生成RGB格式，需要做如下配置,Transparency 设置透明
        newImage = graphics.getDeviceConfiguration().createCompatibleImage(sliderWidth, sliderHeight,
                Transparency.TRANSLUCENT);

        //新建的图像根据模板颜色赋值,源图生成遮罩
        ImageUtil.cutByTemplate(originalImage, sliderImage, newImage, randomX, randomY);

        //设置“抗锯齿”的属性
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        graphics.setStroke(new BasicStroke(BOLD, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL));
        graphics.drawImage(newImage, 0, 0, null);
        graphics.dispose();

        //添加水印
        ImageUtil.addWatermark(originalImage, "请滑动拼图");
        ByteArrayOutputStream newImageOs = new ByteArrayOutputStream();//新建流。
        ImageIO.write(newImage, TEMP_IMG_FILE_TYPE, newImageOs);//利用ImageIO类提供的write方法，将bi以png图片的数据模式写入流。
        byte[] newImagery = newImageOs.toByteArray();

        ByteArrayOutputStream oriImagesOs = new ByteArrayOutputStream();//新建流。
        ImageIO.write(originalImage, IMG_FILE_TYPE, oriImagesOs);//利用ImageIO类提供的write方法，将bi以jpg图片的数据模式写入流。
        byte[] oriImageByte = oriImagesOs.toByteArray();

        pictureMap.put("slidingImage", "data:image/png;base64," + Base64Utils.encodeToString(newImagery));
        pictureMap.put("backImage", "data:image/png;base64," + Base64Utils.encodeToString(oriImageByte));
//       x轴
        pictureMap.put("randomX", randomX);
//       y轴
        pictureMap.put("randomY", randomY);

        pictureMap.put("originalHeight", originalHeight);
        pictureMap.put("originalWidth", originalWidth);
        pictureMap.put("sliderHeight", sliderHeight);
        pictureMap.put("sliderWidth", sliderWidth);
        return pictureMap;
    }


}
