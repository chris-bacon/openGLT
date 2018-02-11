# openGL

This is a graphics engine, using the OpenGL bindings, written in Haskell. 

This is currently a work in progress and an exercise for learning, though I hope one day to make it into something moderately useful and/or interesting.

## OpenGL

Putting things into your own words is a very effective learning mechanism, so this is my attempt to explain OpenGL in my own words. It goes without saying that since this is my interpretation it may be wrong.

### Shaders

First an empty program object is created by the C glCreateProgram function. This is an object to which shaders can be attached. Shaders are created by invoking glCreateShader. This function creates an empty shader object. Note, that in invoking glCreateShader we specify what kind of shader we are creating - Vertex, Fragment, Geometry, etc. We do this by passing that into glCreateShader. Now, if we store our shaders in a file on the hard drive, then we can load it and pass the contents of that shader through to glShaderSource, specifying which shader object we wish to overwrite. We can compile this newly created and written shader object with glCompileShader. We then will need to somehow attach a shader to our empty program object. We do this with glAttachShader, which takes a program object and shader object. This allows a linking operation to be performed later on, linking all of the various shaders to the program. We can use glAttribLocation to, essentially, pass through values to the attributes we create in our shaders (stored on disk). Then, we can link the program, with glLinkProgram. Any shader object of type GL_VERTEX_SHADER attached to the program will be used to create an executable that will run on the vertex processor. Likewise respectively for the other types of shader: Fragment, Geometry, etc. After linking, we can validate the program to check whether the executables created can actually execute within the current OpenGL state.
