using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

public class terrain_height_map_generator : MonoBehaviour
{

    public Texture2D tex;
    public Terrain Ter;

    public int TextureWidth;
    public int TextureHeight;

    public float MaxHeight; //height above that correstponds to 1 brightness

    public bool GenerateOnstart = false;

    private float terrain_width;
    private float terrain_height;

    private float pixel_width;
    private float pixel_height;


    // Start is called before the first frame update
    void Start()
    {
        if (!GenerateOnstart) return;
        terrain_width = Ter.terrainData.size.x;
        terrain_height = Ter.terrainData.size.z;
        pixel_width = terrain_width / TextureWidth;
        pixel_height = terrain_height / TextureHeight;

        byte[] height_array = ReadHeights();

        WriteToRenderTexture(height_array);
    }

    // Update is called once per frame
    void Update()
    {
        ReadHeights();
    }

    byte[] ReadHeights()
    {
        byte[] height_array = new byte[TextureWidth * TextureHeight * 2];

        //float[,] height_array_float = Ter.terrainData.GetHeights(0, 0, TextureWidth, TextureHeight);
        float xpos;
        float zpos;
        float height;
        LayerMask lm = 1 << 8;
        int size = 256 * 256;

        for (int i = 0; i < TextureWidth; i++)
        {
            for (int j = 0; j < TextureHeight; j++)
            {
                //height_array[j * TextureWidth + i] = (byte)Mathf.RoundToInt(height_array_float[i, j] * 256f * HeightMultiplier);
                xpos = (i + 0.5f) * pixel_width;
                zpos = (j + 0.5f) * pixel_height;
                Vector3 world_pos = transform.TransformPoint(new Vector3(xpos, MaxHeight + 0.1f, zpos));
                RaycastHit hit;
                Physics.Raycast(world_pos, Vector3.down,out hit, MaxHeight + 0.5f, lm);
                int byte_pos = 2* (j * TextureWidth  + i);
                int value = Mathf.Clamp(Mathf.RoundToInt((1f - (hit.distance - 0.1f) / MaxHeight) * size), 0, size);
                height_array[byte_pos + 1] = (byte)(value >> 8);
                height_array[byte_pos] = (byte)(value & 0x00FF);
            }
        }

        return height_array;
    }

    void WriteToRenderTexture(byte[] array)
    {
        Destroy(tex);
        tex = new Texture2D(TextureWidth, TextureHeight, TextureFormat.R16, false);
        tex.LoadRawTextureData(array);
        tex.Apply();
        byte[] bytes = tex.EncodeToPNG();
        Destroy(tex); 
        File.WriteAllBytes(Application.dataPath + "/Materials/SavedScreen.png", bytes);

    }



}
